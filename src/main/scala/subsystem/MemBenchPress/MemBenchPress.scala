package subsystem.MemBenchPress
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLBundleA
import freechips.rocketchip.regmapper._
import freechips.rocketchip
import midas.targetutils.SynthesizePrintf
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy.BufferParams.flow
import freechips.rocketchip.tilelink.TLMessages.AccessAck
import freechips.rocketchip.tilelink.TLMessages.AccessAckData
import freechips.rocketchip.diplomacy.{AddressRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.resources.HasReservedAddressRange
import freechips.rocketchip.subsystem.{BaseSubsystem, MBUS, Attachable}
import freechips.rocketchip.subsystem._
import chisel3.util.random.LFSR
import org.chipsalliance.diplomacy.aop.Select
import com.fasterxml.jackson.annotation.JsonProperty.Access
import _root_.subsystem.rme.RMEKey
import _root_.subsystem.rme.RelMemParams



case class MemBenchPressParams (
    attachLLC: Boolean = true,
    maxMLP: Int = 16,
    controlAddress: BigInt = 0x500000L,
    reservedAddressStart: BigInt = 0x160000000L,
    reservedAddressSize: BigInt =  0x10000000L,

)
case object MemBenchPressKey extends Field[Option[MemBenchPressParams]](None)


object AccessPattern extends ChiselEnum {
  val Linear,Strided,Random = Value
}


class MemBenchPress(params: MemBenchPressParams)(implicit p: Parameters) extends LazyModule
{

    val reservedRegion = Seq(AddressSet(params.reservedAddressStart, (math.pow(2,log2Ceil(params.reservedAddressSize))-1).toInt))
    val device = new SimpleDevice("membenchpress",Seq("ku-csl,membenchpress")) with HasReservedAddressRange {}
    /*
      We need this to reserve an address range in the device tree
      -> This required modifications to the device tree generation. See RocketChip fork
    */
    ResourceBinding {
      Resource(device, "reserved").bind(ResourceAddress(reservedRegion, rocketchip.resources.ResourcePermissions(true, true, true, false, true)))
    }



    val ctlNode = TLRegisterNode(
        address = Seq(AddressSet(params.controlAddress, 0xfff)),
        device = new SimpleDevice("mem-bench-press", Seq("mem-bench-press")),
        beatBytes = 8
      )

      val node = TLClientNode(
        Seq(
          TLMasterPortParameters.v1(
            clients = Seq(
              TLMasterParameters.v1(
                name = "mem-bench-press",
                sourceId = IdRange(0, params.maxMLP)
              )
            )
          )
        )
      )

    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) {

      //val (in, tlInEdge) = node.in(0)
      val (out, tlOutEdge) = node.out(0)
      val addressBits = log2Ceil(params.reservedAddressSize)
      val requestReservations = RegInit(VecInit(Seq.fill(params.maxMLP)(false.B)))
      val nextAddress = RegInit(0.U(addressBits.W))
      val accessPattern = RegInit(0.U(2.W))
      val rst = RegInit(false.B)
      val active = RegInit(false.B)
      val stride = RegInit(0.U(addressBits.W))
      val reqCount = RegInit(0.U(32.W))
      val write = RegInit(false.B)
      val mlpSet = RegInit(params.maxMLP.U(log2Ceil(params.maxMLP + 1).W))
      val setBits =  RegInit(0.U(addressBits.W))
      val unsetBits =  RegInit(0.U(addressBits.W))
      val doRowConflict = RegInit(false.B)
      val rowConflictBit = RegInit(0.U(addressBits.W))
      val rowSelect = RegInit(0.U(32.W))
      val rowBitStart = RegInit(0.U(log2Ceil(32).W))




      val mmioAccessPattern = Seq(0x00 -> Seq(
        RegField(accessPattern.getWidth, accessPattern,
          RegFieldDesc("accessPattern", "Memory access pattern: 0=linear, 1=strided, 2=random, 3=bank"))))
      val mmioReset = Seq(0x08 -> Seq(
        RegField(rst.getWidth, rst,
          RegFieldDesc("reset", "Reset the memory benchmark state"))))
      val mmioActive = Seq(0x10 -> Seq(
        RegField(active.getWidth, active,
          RegFieldDesc("active", "Enable memory benchmark requests"))))
      val mmioStride = Seq(0x18 -> Seq(
        RegField(stride.getWidth, stride,
          RegFieldDesc("stride", "Stride between memory requests"))))
      val mmioRequestCount = Seq(0x20 -> Seq(
        RegField(reqCount.getWidth, reqCount,
          RegFieldDesc("requestCount", "Number of memory requests remaining"))))
      val mmioDoWrite = Seq(0x28 -> Seq(
        RegField(write.getWidth, write,
          RegFieldDesc("DoWrite", "Issue write requests"))))
      val mmioSetMLP = Seq(0x30 -> Seq(
        RegField(mlpSet.getWidth, mlpSet,
          RegFieldDesc("mlpSet", "Set the MLP"))))
      val mmioMaskSet = Seq(0x38 -> Seq(
        RegField(setBits.getWidth, setBits,
          RegFieldDesc("SetBits", "Set bits mask"))))
      val mmioMaskUnSet = Seq(0x40 -> Seq(
        RegField(unsetBits.getWidth, unsetBits,
          RegFieldDesc("UnsetBits", "Unset bits mask"))))
      val mmioRowBitStart = Seq(0x48 -> Seq(
        RegField(rowBitStart.getWidth, rowBitStart,
          RegFieldDesc("rowBitStart", "rowBitStart"))))



      val mmioRegisters = mmioAccessPattern ++ mmioReset ++ mmioActive ++
        mmioStride ++ mmioRequestCount ++ mmioDoWrite ++ mmioSetMLP ++ mmioMaskSet ++ mmioMaskUnSet ++ mmioRowBitStart
      ctlNode.regmap(mmioRegisters: _*)





            val (a_first, a_last, a_done, a_count) = tlOutEdge.firstlastHelper(out.a.bits, out.a.fire)
      val rand = LFSR(addressBits, out.a.fire && a_last)
      val maxRow = (params.reservedAddressSize - 1).U(addressBits.W) >> rowBitStart



      def GetAddressMask(base_addr: UInt) : UInt = {
        val maskedAddress = Wire(UInt(addressBits.W))
        maskedAddress := ((base_addr & ~unsetBits) | setBits) + (rowSelect << rowBitStart)
        maskedAddress & ~63.U(addressBits.W)
      }


      def MakeOutBoundRequest(src : UInt, addr: UInt) : TLBundleA = {
              val (legal, ret) = tlOutEdge.Get(          // use the edge you already have!
                fromSource = src,
                toAddress  = addr + params.reservedAddressStart.U,
                lgSize     = 6.U
            )
            // legal should be true — add assert(legal) in synthesis if you want
            ret  // the helper already sets opcode, param, size, address, mask, data=0, corrupt=false, etc. correctly
      }


      def MakeOutBoundRequestWrite(src:  UInt, addr: UInt, data: UInt) : TLBundleA = {
        val (legal, ret) = tlOutEdge.Put(
            fromSource = src,
            toAddress  = addr + params.reservedAddressStart.U,
            lgSize     = 6.U, // 64-byte transaction
            data       = data
        )
            ret
        }

    def AvailableSources(): Seq[Bool] = {
        requestReservations.zipWithIndex.map {
            case (reserved, i) => !reserved && i.U < mlpSet
        }
    }

    def CanSendRequest(): Bool = {
        AvailableSources().reduce(_ || _) &&
        reqCount =/= 0.U &&
        active
    }

    def SelectedSource(): UInt = {
        PriorityEncoder(AvailableSources())
    }


      val burstActive = RegInit(false.B)
      val activeSrc = RegInit(0.U(log2Ceil(params.maxMLP).W))


      def FreeRequestReservation(src : UInt) : Unit = {
        assert(requestReservations(src))
        requestReservations(src) := false.B
      }

      def AllocRequestReservation(src: UInt) : Unit = {
        assert(!requestReservations(src))
        requestReservations(src) := true.B
      }

      def GetAddress() : UInt = {
        nextAddress
      }

      def GetNextAddress() : UInt = {
        val ret = WireInit(0.U(addressBits.W))
        switch (accessPattern) {
          is (0.U)
          {
            ret := GetAddress() + 0x40.U
          }

          is (1.U) {
            ret := GetAddress() + stride
          }

          is (2.U) {
            ret := rand
          }

          is (3.U) {
            ret := rand
          }
        }

        Mux(ret >= params.reservedAddressSize.U, 0.U, ret & ~63.U(addressBits.W)) // align to cache line
      }




      val selSrc = SelectedSource()
      val outSelectedSrc = Mux(burstActive, activeSrc, selSrc)
      val outSelectedAddr = Mux(accessPattern === 3.U, GetAddressMask(GetAddress()), GetAddress())
      when (out.a.fire && a_last && accessPattern === 3.U) {
        rowSelect := Mux(rowSelect < maxRow, rowSelect + 1.U, 0.U)
      }
      out.a.bits := Mux(
        burstActive || write,
        MakeOutBoundRequestWrite(outSelectedSrc, outSelectedAddr, 0.U),
        MakeOutBoundRequest(outSelectedSrc, outSelectedAddr)
      )
      // Once a multibeat Put starts, finish it even if active is cleared or a
      // software reset is requested. Do not start a new request while reset is pending.
      out.a.valid := burstActive || (CanSendRequest() && !rst)

      out.d.ready := true.B
      out.b.ready := true.B
      out.c.valid := false.B
      out.e.valid := false.B



      when (out.a.fire && a_first) {
        activeSrc := outSelectedSrc
        AllocRequestReservation(outSelectedSrc)

        when (!a_last) {
          burstActive := true.B
        }
      }

      when (out.a.fire && a_last)
      {
        burstActive := false.B
        nextAddress := GetNextAddress()
        reqCount := reqCount - 1.U
        SynthesizePrintf("FiredOut. isPut %d, address %d, ReqCount %d\n", out.a.bits.opcode === TLMessages.PutFullData, out.a.bits.address, reqCount)
      }



      val (d_first, d_last, d_done, d_count) = tlOutEdge.firstlastHelper(out.d.bits, out.d.fire)
      when (out.d.fire && d_last)
      {
        FreeRequestReservation(out.d.bits.source)
      }

      // Keep active asserted until every issued request has received its response.
      // This makes active a reliable completion indicator for software.
      when (active && reqCount === 0.U && !requestReservations.reduce(_ || _) && !burstActive)
      {
        active := false.B
      }

      // Each newly configured run starts from row zero and a deterministic raw
      // address. Preserve the address while completing an in-flight Put burst.
      when (!active && !burstActive)
      {
        nextAddress := 0.U
        rowSelect := 0.U
      }


      when (rst && !burstActive)
      {
        active := false.B
        reqCount := 0.U
        accessPattern := 0.U
        stride := 0.U
        rst := false.B
        nextAddress := 0.U
        write := false.B
        burstActive := false.B
        mlpSet := params.maxMLP.U
        setBits := 0.U
        unsetBits := 0.U
        rowBitStart := 0.U
        rowSelect := 0.U
      }


    }

}


trait CanHavePeripheryMemBenchPress { this: BaseSubsystem =>
  private val portName = "MemBenchPress"
  private val memBenchPbus = locateTLBusWrapper(PBUS)
  private val memBenchMbus = locateTLBusWrapper(MBUS)
  private val memBenchSbus = locateTLBusWrapper(SBUS)


  val membenchpress = p(MemBenchPressKey) match {
    case Some(params: MemBenchPressParams) => {

      val memBenchPress = if (params.attachLLC) {
        memBenchSbus { LazyModule(new MemBenchPress(params)) }
      } else {
        memBenchMbus { LazyModule(new MemBenchPress(params)) }
      }

      if (params.attachLLC) {
        // DTU -> SBUS -> LLC -> MBUS -> DRAM
        memBenchSbus.coupleFrom("dtu_to_llc") {
          _ := TLBuffer(1) := memBenchPress.node
        }
      } else {
        // DTU -> MBUS -> DRAM
        // Bypasses LLC
        memBenchMbus.coupleFrom("dtu_to_mem") {
          _ := TLBuffer(1) := memBenchPress.node
        }
      }

    memBenchPbus.coupleTo("mem_bench_press_ctl") {
      memBenchPress.ctlNode := TLFragmenter(memBenchPbus.beatBytes, memBenchPbus.blockBytes) := _
    }

    None
      //mbus.rme.get
    }
    case None => None
}
}

class WithMemBenchPress(attach_to_llc : Boolean = true) extends Config((site, here, up) => {
  case MemBenchPressKey => Some(MemBenchPressParams(attachLLC = attach_to_llc))
})

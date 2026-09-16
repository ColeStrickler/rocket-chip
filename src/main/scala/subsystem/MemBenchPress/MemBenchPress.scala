package subsystem.MemBenchPress
package subsystem.rme
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
import freechips.rocketchip.subsystem.{BaseSubsystem, MBUS, Attachable}
import freechips.rocketchip.subsystem._


case class MemBenchPressParams (
    attachLLC: Boolean = true,

)
case object MemBenchPressKey extends Field[Option[MemBenchPressParams]](None)


class MemBenchPress(params: MemBenchPressParams)(implicit p: Parameters) extends LazyModule
{

    lazy val module = new Impl
    class Impl extends LazyModuleImp(this) {


    }

}


trait CanHavePeripheryMemBenchPress { this: BaseSubsystem =>
  private val portName = "MemBenchPress"
  val pbus = locateTLBusWrapper(PBUS)
  val mbus = locateTLBusWrapper(MBUS)
  val fbus = locateTLBusWrapper((FBUS))
  val sbus = locateTLBusWrapper(SBUS)


  val rme = p(MemBenchPressKey) match {
    case Some(params: MemBenchPressParams) => {



        if (params.attachLLC)
        {

        }
        else
        {

        }




    sbus.coupleTo("dtu_uncached") {
      mbus.dtu_uncached_region.get.cpuNode := 
      TLBuffer(1)  :=  _
    }
      // Connect uncached region to memory bus (MBUS) := TLFragmenter(mbus.beatBytes, mbus.blockBytes, holdFirstDeny=true)

    mbus.coupleFrom("simple_uncached_region_mem") { _ := mbus.dtu_uncached_region.get.memNode }
          
    None
      //mbus.rme.get
    }
    case None => None
}
  }

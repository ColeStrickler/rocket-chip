package subsystem.rme
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tilelink.TLBundleA
import freechips.rocketchip.regmapper._
import midas.targetutils.SynthesizePrintf
import org.chipsalliance.cde.config.{Parameters, Field, Config}
import freechips.rocketchip.diplomacy.BufferParams.flow


case class RelMemParams (
    regaddress: Int,
    rmeaddress: BigInt,
)


class RME(params: RelMemParams)(implicit p: Parameters) extends LazyModule
{
  val device = new SimpleDevice("relmem",Seq("ku-csl,relmem"))

  val node = TLAdapterNode()

  val regnode = new TLRegisterNode(
    address = Seq(AddressSet(params.regaddress, 0x7ff)),
    device = device,
    beatBytes = 8)



    def ToRME(addr : UInt) : Bool = {
        val torme : Bool = addr > params.rmeaddress.U
        torme
    }
     
  
  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val nClients = node.in.length
    println(s"Number of edges into RME: $nClients ${node.in}")
    require(nClients >= 1)
    val (out, out_edge) = node.out(0)
    val (in, in_edge) = node.in(0)
    val outParams = out_edge.bundle
    val inParams = in_edge.bundle
    val memBase = p(ExtMem).get.master.base.U
    // last connect semantics should work for us here
    out <> in


    // All incoming requests are channel A right? So we should only give D responses
    val returnArbiter = Module(new RRArbiter(new TLBundleD(inParams), 2))

    when(ToRME(in.a.bits.address))
    {
        /*
            lets capture these and send back dummy data 
        */


    }
    .otherwise
    {
        // otherwise send on to memory


    }

  }

}

trait CanHaveRME {
  val rme: Option[RME]
}
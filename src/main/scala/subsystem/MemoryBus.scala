// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.devices.tilelink.{BuiltInDevices, HasBuiltInDeviceParams, BuiltInErrorDeviceParams, BuiltInZeroDeviceParams}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.Location
import subsystem.rme.RME
import subsystem.rme.RelMemParams
import midas.targetutils.SynthesizePrintf
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy.IdRange

/** Parameterization of the memory-side bus created for each memory channel */
case class MemoryBusParams(
  beatBytes: Int,
  blockBytes: Int,
  dtsFrequency: Option[BigInt] = None,
  zeroDevice: Option[BuiltInZeroDeviceParams] = None,
  errorDevice: Option[BuiltInErrorDeviceParams] = None,
  replication: Option[ReplicatedRegion] = None)
  extends HasTLBusParams
  with HasBuiltInDeviceParams
  with HasRegionReplicatorParams
  with TLBusWrapperInstantiationLike
{
  def instantiate(context: HasTileLinkLocations, loc: Location[TLBusWrapper])(implicit p: Parameters): MemoryBus = {
    val mbus = LazyModule(new MemoryBus(this, loc.name))
    mbus.suggestName(loc.name)
    context.tlBusWrapperLocationMap += (loc -> mbus)
    mbus
  }
}

/** Wrapper for creating TL nodes from a bus connected to the back of each mem channel */
class MemoryBus(params: MemoryBusParams, name: String = "memory_bus")(implicit p: Parameters)
    extends TLBusWrapper(params, name)(p)
{
  private val replicator = params.replication.map(r => LazyModule(new RegionReplicator(r)))
  val prefixNode = replicator.map { r =>
    r.prefix := addressPrefixNexusNode
    addressPrefixNexusNode
  }
  //val rme = None
 val rme = Some(LazyModule(new RME(RelMemParams())))
  

  
  
  val xbar = LazyModule(new TLXbar(nameSuffix = Some(name))).suggestName(busName + "_xbar")
  val inwardNode: TLInwardNode =
    replicator.map(xbar.node :*=* TLFIFOFixer(TLFIFOFixer.all) :*=* _.node)
        .getOrElse(xbar.node :*=* TLFIFOFixer(TLFIFOFixer.all))


  /*
    We expand the range of source IDs available for the RME so we can multiply requests, and
    send them concurrently
  */
  val numFetchUnits = 32
  private val client = TLMasterParameters.v1(
    name     = "TLSourceExpander",
    sourceId = IdRange(0, numFetchUnits)
    )
  val node = (new TLAdapterNode(
    clientFn  = { cp => 
      // We erase all client information since we crush the source Ids
      TLMasterPortParameters.v1(
        clients = Seq(client.v1copy(requestFifo = cp.clients.exists(_.requestFifo))),
        echoFields = cp.echoFields,
        requestFields = cp.requestFields,
        responseKeys = cp.responseKeys)
    },
    managerFn = { mp => mp.v1copy(managers = mp.managers.map(m => m.v1copy(fifoId = if (numFetchUnits==1) Some(0) else m.fifoId)))
    }) {
    //override def circuitIdentity = edges.in.map(_.client).forall(noShrinkRequired)
  })
 
  
 // rme.get.manager :*= xbar.node


  /* 
    Is this the correct way to do this?
  */
  //coupleTo("rme-manager") {rme.get.manager := _ }
  //coupleFrom()
    /* 
      Can we make the :*= into a := ?
     */
    
  
  val outwardNode: TLOutwardNode = rme.get.node :*= ProbePicker() :*= xbar.node
  //val outwardNode: TLOutwardNode = rme.get.node :*= node :*= ProbePicker() :*= xbar.node
  //val outwardNode: TLOutwardNode = ProbePicker() :*= xbar.node --> default
  // coupleTo("rme-manager"){ rme.get.manager := TLFragmenter(beatBytes, blockBytes) := _ }
  def busView: TLEdge = xbar.node.edges.in.head

  val builtInDevices: BuiltInDevices = BuiltInDevices.attach(params, outwardNode)
}

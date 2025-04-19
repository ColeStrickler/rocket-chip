// See LICENSE.SiFive for license details.

package freechips.rocketchip.subsystem

import org.chipsalliance.cde.config._
import org.chipsalliance.diplomacy.lazymodule._

import freechips.rocketchip.devices.tilelink.{BuiltInDevices, HasBuiltInDeviceParams, BuiltInErrorDeviceParams, BuiltInZeroDeviceParams}
import freechips.rocketchip.tilelink.{
  ReplicatedRegion, HasTLBusParams, HasRegionReplicatorParams, TLBusWrapper,
  TLBusWrapperInstantiationLike, RegionReplicator, TLXbar, TLInwardNode,
  TLOutwardNode, ProbePicker, TLEdge, TLFIFOFixer
}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.Location
import subsystem.rme.RME
import subsystem.rme.RelMemParams
import midas.targetutils.SynthesizePrintf
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy.IdRange
import subsystem.rme.subsystem.rme.TLSourceExpander



import freechips.rocketchip.util.Location

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
  val rme = Some(LazyModule(new RME(RelMemParams())))
  private val xbar = LazyModule(new TLXbar(nameSuffix = Some(name))).suggestName(busName + "_xbar")



  /*
    We expand the range of source IDs available for the RME so we can multiply requests, and
    send them concurrently
  */
  val numFetchUnits = 16

 // rme.get.manager :*= xbar.node

  val inwardNode: TLInwardNode =
    replicator.map(xbar.node :*=* TLFIFOFixer(TLFIFOFixer.all) :*=* _.node)
        .getOrElse(xbar.node :*=* TLFIFOFixer(TLFIFOFixer.all))

  val outwardNode: TLOutwardNode = rme.get.node :*= TLSourceExpander(numFetchUnits*4).node :*= ProbePicker()  :*= xbar.node
  def busView: TLEdge = xbar.node.edges.in.head

  val builtInDevices: BuiltInDevices = BuiltInDevices.attach(params, outwardNode)
}

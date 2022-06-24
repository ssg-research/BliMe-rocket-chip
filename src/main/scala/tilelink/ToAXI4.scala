// See LICENSE.SiFive for license details.

package freechips.rocketchip.tilelink

import Chisel._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba._

class AXI4TLStateBundle(val sourceBits: Int) extends Bundle {
  val size   = UInt(width = 4)
  val source = UInt(width = sourceBits max 1)
}

case object AXI4TLState extends ControlKey[AXI4TLStateBundle]("tl_state")
case class AXI4TLStateField(sourceBits: Int) extends BundleField(AXI4TLState) {
  def data = Output(new AXI4TLStateBundle(sourceBits))
  def default(x: AXI4TLStateBundle) = {
    x.size   := 0.U
    x.source := 0.U
  }
}

/** TLtoAXI4IdMap serves as a record for the translation performed between id spaces.
  *
  * Its member [axi4Masters] is used as the new AXI4MasterParameters in diplomacy.
  * Its member [mapping] is used as the template for the circuit generated in TLToAXI4Node.module.
  */
class TLtoAXI4IdMap(tlPort: TLMasterPortParameters) extends IdMap[TLToAXI4IdMapEntry]
{
  val tlMasters = tlPort.masters.sortBy(_.sourceId).sortWith(TLToAXI4.sortByType)
  private val axi4IdSize = tlMasters.map { tl => if (tl.requestFifo) 1 else tl.sourceId.size }
  private val axi4IdStart = axi4IdSize.scanLeft(0)(_+_).init
  val axi4Masters = axi4IdStart.zip(axi4IdSize).zip(tlMasters).map { case ((start, size), tl) =>
    AXI4MasterParameters(
      name      = tl.name,
      id        = IdRange(start, start+size),
      aligned   = true,
      maxFlight = Some(if (tl.requestFifo) tl.sourceId.size else 1),
      nodePath  = tl.nodePath)
  }

  private val axi4IdEnd = axi4Masters.map(_.id.end).max
  private val axiDigits = String.valueOf(axi4IdEnd-1).length()
  private val tlDigits = String.valueOf(tlPort.endSourceId-1).length()
  protected val fmt = s"\t[%${axiDigits}d, %${axiDigits}d) <= [%${tlDigits}d, %${tlDigits}d) %s%s%s"

  val mapping: Seq[TLToAXI4IdMapEntry] = tlMasters.zip(axi4Masters).map { case (tl, axi) =>
    TLToAXI4IdMapEntry(axi.id, tl.sourceId, tl.name, tl.supports.probe, tl.requestFifo)
  }
}

case class TLToAXI4IdMapEntry(axi4Id: IdRange, tlId: IdRange, name: String, isCache: Boolean, requestFifo: Boolean)
  extends IdMapEntry
{
  val from = tlId
  val to = axi4Id
  val maxTransactionsInFlight = Some(tlId.size)
}

case class TLToAXI4Node(wcorrupt: Boolean = true)(implicit valName: ValName) extends MixedAdapterNode(TLImp, AXI4Imp)(
  dFn = { p =>
    AXI4MasterPortParameters(
      masters    = (new TLtoAXI4IdMap(p)).axi4Masters,
      requestFields = (if (wcorrupt) Seq(AMBACorruptField()) else Seq()) ++ p.requestFields.filter(!_.isInstanceOf[AMBAProtField]),
      echoFields    = AXI4TLStateField(log2Ceil(p.endSourceId)) +: p.echoFields,
      responseKeys  = p.responseKeys)
  },
  uFn = { p => TLSlavePortParameters.v1(
    managers = p.slaves.map { case s =>
      TLSlaveParameters.v1(
        address            = s.address,
        resources          = s.resources,
        regionType         = s.regionType,
        executable         = s.executable,
        nodePath           = s.nodePath,
        supportsGet        = s.supportsRead,
        supportsPutFull    = s.supportsWrite,
        supportsPutPartial = s.supportsWrite,
        fifoId             = Some(0),
        mayDenyPut         = true,
        mayDenyGet         = true)},
      beatBytes = p.beatBytes,
      minLatency = p.minLatency,
      responseFields = p.responseFields,
      requestKeys    = AMBAProt +: p.requestKeys)
  })

// wcorrupt alone is not enough; a slave must include AMBACorrupt in the slave port's requestKeys
class TLToAXI4(val combinational: Boolean = true, val adapterName: Option[String] = None, val stripBits: Int = 0, val wcorrupt: Boolean = true)(implicit p: Parameters) extends LazyModule
{
  require(stripBits == 0, "stripBits > 0 is no longer supported on TLToAXI4")
  val node = TLToAXI4Node(wcorrupt)

  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val slaves  = edgeOut.slave.slaves

      // All pairs of slaves must promise that they will never interleave data
      require (slaves(0).interleavedId.isDefined)
      slaves.foreach { s => require (s.interleavedId == slaves(0).interleavedId) }

      // Construct the source=>ID mapping table
      val map = new TLtoAXI4IdMap(edgeIn.client)
      val sourceStall = Wire(Vec(edgeIn.client.endSourceId, Bool()))
      val sourceTable = Wire(Vec(edgeIn.client.endSourceId, out.aw.bits.id))
      val idStall = Wire(init = Vec.fill(edgeOut.master.endId) { Bool(false) })
      var idCount = Array.fill(edgeOut.master.endId) { None:Option[Int] }

      map.mapping.foreach { case TLToAXI4IdMapEntry(axi4Id, tlId, _, _, fifo) =>
        for (i <- 0 until tlId.size) {
          val id = axi4Id.start + (if (fifo) 0 else i)
          sourceStall(tlId.start + i) := idStall(id)
          sourceTable(tlId.start + i) := UInt(id)
        }
        if (fifo) { idCount(axi4Id.start) = Some(tlId.size) }
      }

      adapterName.foreach { n =>
        println(s"$n AXI4-ID <= TL-Source mapping:\n${map.pretty}\n")
        ElaborationArtefacts.add(s"$n.axi4.json", s"""{"mapping":[${map.mapping.mkString(",")}]}""")
      }

      val memPortParams = p(ExtMem).get
      val dramSize = memPortParams.master.size

      def getDataAddr(a_address : UInt) = {
        val sizeOfBlindednessTable = (dramSize/8).U
        a_address + sizeOfBlindednessTable
      }

      def getBlindmaskAddr(a_address : UInt) = {
        a_address >> 3 // divide by 8
      }

      def getBlindmaskMask(a_address : UInt) = {
        
      }

      // We need to keep the following state from A => D: (size, source)
      // All of those fields could potentially require 0 bits (argh. Chisel.)
      // We will pack all of that extra information into the echo bits.

      require (log2Ceil(edgeIn.maxLgSize+1) <= 4)
      val a_queue = Queue(in.a, entries=1, pipe=true, flow=true)
      // val addr_wire = Wire(Decoupled(UInt()))
      // addr_wire.valid := true.B
      // addr_wire.bits  := edgeIn.address(in.a.bits)
      // a_addr_queue = Queue(addr_wire, entries=1, pipe=true, flow=true)
      // val a_address = a_addr_queue.bits

      val a_address = edgeIn.address(a_queue.bits)
      val a_dataAddr = getDataAddr(a_address)
      val a_blindmaskAddr = getBlindmaskAddr(a_address)
      val a_source  = a_queue.bits.source
      val a_size    = edgeIn.size(a_queue.bits) // TODO
      val a_isPut   = edgeIn.hasData(a_queue.bits)
      val (a_first, a_last, _) = edgeIn.firstlast(a_queue)

      val r_state = out.r.bits.echo(AXI4TLState)
      val r_source  = r_state.source
      val r_size    = r_state.size

      val b_state = out.b.bits.echo(AXI4TLState)
      val b_source  = b_state.source
      val b_size    = b_state.size

      // We need these Queues because AXI4 queues are irrevocable
      val depth = if (combinational) 1 else 2
      val out_arw = Wire(Decoupled(new AXI4BundleARW(out.params)))
      val out_w = Wire(out.w)
      out.w :<> Queue.irrevocable(out_w, entries=depth, flow=combinational)
      val queue_arw = Queue.irrevocable(out_arw, entries=depth, flow=combinational)

      // Fan out the ARW channel to AR and AW
      out.ar.bits := queue_arw.bits
      out.aw.bits := queue_arw.bits
      out.ar.valid := queue_arw.valid && !queue_arw.bits.wen
      out.aw.valid := queue_arw.valid &&  queue_arw.bits.wen
      queue_arw.ready := Mux(queue_arw.bits.wen, out.aw.ready, out.ar.ready)

      val beatBytes = edgeIn.manager.beatBytes
      val maxSize   = UInt(log2Ceil(beatBytes))
      val beatBits  = beatBytes*8

      // BlindedMem buffers and signals:
      val zero_blindedmem = Wire(BlindedMem(UInt(beatBits.W), UInt(beatBytes.W)))
      zero_blindedmem.bits := 0.U
      zero_blindedmem.blindmask := 0.U
      val blindedmem_reg = RegInit(zero_blindedmem)
      val blindmask_get_inflight = RegInit(false.B)
      val start_blindmask_get = Wire(Bool())
      val blindmask_phase = Wire(Bool())
      
      val doneAW    = RegInit(Bool(false))
      when (a_queue.fire()) { doneAW := !a_last }

      val arw = out_arw.bits
      arw.wen   := a_isPut
      arw.id    := sourceTable(a_source)
      arw.addr  := Mux(blindmask_phase, a_blindmaskAddr, a_dataAddr)
      arw.len   := UIntToOH1(a_size, AXI4Parameters.lenBits + log2Ceil(beatBytes)) >> log2Ceil(beatBytes)
      arw.size  := Mux(a_size >= maxSize, maxSize, a_size) // TODO
      arw.burst := AXI4Parameters.BURST_INCR
      arw.lock  := UInt(0) // not exclusive (LR/SC unsupported b/c no forward progress guarantee)
      arw.cache := UInt(0) // do not allow AXI to modify our transactions
      arw.prot  := AXI4Parameters.PROT_PRIVILEDGED
      arw.qos   := UInt(0) // no QoS
      arw.user :<= a_queue.bits.user
      arw.echo :<= a_queue.bits.echo
      val a_extra = arw.echo(AXI4TLState)
      a_extra.source := a_source
      a_extra.size   := a_size

      a_queue.bits.user.lift(AMBAProt).foreach { x =>
        val prot  = Wire(Vec(3, Bool()))
        val cache = Wire(Vec(4, Bool()))
        prot(0) :=  x.privileged
        prot(1) := !x.secure
        prot(2) :=  x.fetch
        cache(0) := x.bufferable
        cache(1) := x.modifiable
        cache(2) := x.readalloc
        cache(3) := x.writealloc
        arw.prot  := Cat(prot.reverse)
        arw.cache := Cat(cache.reverse)
      }

      val stall = sourceStall(a_queue.bits.source) && a_first
      val in_a_ready = WireDefault(!stall && Mux(a_isPut, (doneAW || out_arw.ready) && out_w.ready, out_arw.ready) && !blindmask_phase)
      a_queue.ready := in_a_ready
      // a_addr_queue.ready := in_a_ready
      out_arw.valid := !stall && Mux(blindmask_phase, true.B, a_queue.valid) && Mux(a_isPut, !doneAW && out_w.ready, Bool(true))

      out_w.valid := !stall && a_queue.valid && a_isPut && (doneAW || out_arw.ready)
      out_w.bits.data := Mux(blindmask_phase, , a_queue.bits.data(beatBits-1, 0))
      out_w.bits.strb := a_queue.bits.mask // TODO
      out_w.bits.last := a_last
      out_w.bits.user.lift(AMBACorrupt).foreach { _ := a_queue.bits.corrupt } // TODO

      // R and B => D arbitration
      val r_holds_d = RegInit(Bool(false))
      when (out.r.fire()) { r_holds_d := !out.r.bits.last }
      // Give R higher priority than B, unless B has been delayed for 8 cycles
      val b_delay = Reg(UInt(width=3))
      when (out.b.valid && !out.b.ready) {
        b_delay := b_delay + UInt(1)
      } .otherwise {
        b_delay := UInt(0)
      }
      val r_wins = (out.r.valid && b_delay =/= UInt(7)) || r_holds_d

      out.r.ready := in.d.ready && r_wins
      out.b.ready := in.d.ready && !r_wins
      val curr_dresp_valid = Mux(r_wins, out.r.valid, out.b.valid)
      start_blindmask_get := curr_dresp_valid && !blindmask_get_inflight
      blindmask_phase := start_blindmask_get || blindmask_get_inflight
      when (curr_dresp_valid) {
        blindmask_get_inflight := !blindmask_get_inflight
      }
      
      in.d.valid := curr_dresp_valid && blindmask_get_inflight // pass combined blindedmem word as soon as blindmask arrives

      // If the first beat of the AXI RRESP is RESP_DECERR, treat this as a denied
      // request. We must pulse extend this value as AXI is allowed to change the
      // value of RRESP on every beat, and ChipLink may not.
      val r_first = RegInit(Bool(true))
      when (out.r.fire()) { r_first := out.r.bits.last }
      val r_denied  = out.r.bits.resp === AXI4Parameters.RESP_DECERR holdUnless r_first
      val r_corrupt = out.r.bits.resp =/= AXI4Parameters.RESP_OKAY
      val b_denied  = out.b.bits.resp =/= AXI4Parameters.RESP_OKAY

      val r_d = edgeIn.AccessAck(r_source, r_size, UInt(0), denied = r_denied, corrupt = r_corrupt || r_denied)
      val b_d = edgeIn.AccessAck(b_source, b_size, denied = b_denied)
      r_d.user :<= out.r.bits.user
      r_d.echo :<= out.r.bits.echo
      b_d.user :<= out.b.bits.user
      b_d.echo :<= out.b.bits.echo

      in.d.bits := Mux(r_wins, r_d, b_d)
      in.d.bits.data := out.r.bits.data // avoid a costly Mux

      // We need to track if any reads or writes are inflight for a given ID.
      // If the opposite type arrives, we must stall until it completes.
      val a_sel = UIntToOH(arw.id, edgeOut.master.endId).asBools
      val d_sel = UIntToOH(Mux(r_wins, out.r.bits.id, out.b.bits.id), edgeOut.master.endId).asBools
      val d_last = Mux(r_wins, out.r.bits.last, Bool(true))
      // If FIFO was requested, ensure that R+W ordering is preserved
      (a_sel zip d_sel zip idStall zip idCount) foreach { case (((as, ds), s), n) =>
        // AXI does not guarantee read vs. write ordering. In particular, if we
        // are in the middle of receiving a read burst and then issue a write,
        // the write might affect the read burst. This violates FIFO behaviour.
        // To solve this, we must wait until the last beat of a burst, but this
        // means that a TileLink master which performs early source reuse can
        // have one more transaction inflight than we promised AXI; stall it too.
        val maxCount = n.getOrElse(1)
        val count = RegInit(UInt(0, width = log2Ceil(maxCount + 1)))
        val write = Reg(Bool())
        val idle = count === UInt(0)

        val inc = as && out_arw.fire()
        val dec = ds && d_last && in.d.fire()
        count := count + inc.asUInt - dec.asUInt

        assert (!dec || count =/= UInt(0))        // underflow
        assert (!inc || count =/= UInt(maxCount)) // overflow

        when (inc) { write := arw.wen }
        // If only one transaction can be inflight, it can't mismatch
        val mismatch = if (maxCount > 1) { write =/= arw.wen } else { Bool(false) }
        s := (!idle && mismatch) || (count === UInt(maxCount))
      }

      // Tie off unused channels
      in.b.valid := Bool(false)
      in.c.ready := Bool(true)
      in.e.ready := Bool(true)
    }
  }
}

object TLToAXI4
{
  def apply(combinational: Boolean = true, adapterName: Option[String] = None, stripBits: Int = 0, wcorrupt: Boolean = true)(implicit p: Parameters) =
  {
    val tl2axi4 = LazyModule(new TLToAXI4(combinational, adapterName, stripBits, wcorrupt))
    tl2axi4.node
  }

  def sortByType(a: TLMasterParameters, b: TLMasterParameters): Boolean = {
    if ( a.supports.probe && !b.supports.probe) return false
    if (!a.supports.probe &&  b.supports.probe) return true
    if ( a.requestFifo    && !b.requestFifo   ) return false
    if (!a.requestFifo    &&  b.requestFifo   ) return true
    return false
  }
}

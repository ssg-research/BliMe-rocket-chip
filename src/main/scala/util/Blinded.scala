package freechips.rocketchip.util

import Chisel._

class Blinded[+T <: Data](gen: T) extends Bundle {

  /** A bit that will be asserted when `bits` is blinded
    * @group Signals
    */
  val blinded = Output(Bool())

  /** The data to be transferred
    * @group Signals
    */
  val bits = Output(gen)
}

object Blinded {

  /** Wrap some [[Data]] in a blinded interface
    * @tparam T the type of the data to wrap
    * @param gen the data to wrap
    * @return the wrapped input data
    */
  def apply[T <: Data](gen: T): Blinded[T] = new Blinded(gen)
}

class BlindedMem[+T <: Data, +M <: Bits](gen: T, mask: M) extends Bundle {

  /** A bit mask representing which bytes in `bits` are blinded
    * blindmask(0) is for the LSByte and blindmask(n-1) is for the MSByte
    * @group Signals
    */
  // val blindmask = Output(VecInit(Seq.fill(n)(false.B)))
  val blindmask = Output(mask)

  /** The data to be transferred
    * @group Signals
    */
  val bits = Output(gen)
}

object BlindedMem {

  /** Wrap some [[Data]] in a blinded interface
    * @tparam T the type of the data to wrap
    * @param gen the data to wrap
    * @return the wrapped input data
    */
  def apply[T <: Data, M <: Bits](gen: T, mask: M): BlindedMem[T, M] = new BlindedMem(gen, mask)
}
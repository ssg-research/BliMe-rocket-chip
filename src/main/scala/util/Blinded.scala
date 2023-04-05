package freechips.rocketchip.util

import chisel3._

// import freechips.rocketchip.config.Parameters
// import freechips.rocketchip.tile.ClTagGranule

class Blinded[+T <: Data](gen: T) extends Bundle {

  val clTag = Bits(Blinded.CL_TAG_SIZE.W)

  /** A bit that will be asserted when `bits` is blinded
    * @group Signals
    */
  // var blinded = Bool()

  /** The data to be transferred
    * @group Signals
    */
  val bits = gen.cloneType.asInstanceOf[T]
}

object Blinded {

  val CL_TAG_SIZE: Int = 8

  /** Wrap some [[Data]] in a blinded interface
    * @tparam T the type of the data to wrap
    * @param gen the data to wrap
    * @return the wrapped input data
    */
  def apply[T <: Data](gen: T): Blinded[T] = new Blinded(gen)
}

class BlindedMem[+T <: Data](gen: T, numTags: Int) extends Bundle {

  require((gen.getWidth / 8) % BlindedMem.CL_TAG_GRANULE_SZ == 0 || ((gen.getWidth / 8) < BlindedMem.CL_TAG_GRANULE_SZ))

  // val clTags = Vec(math.max(gen.getWidth / 8 / p(ClTagGranule), 1), Bits(Blinded.CL_TAG_SIZE.W))
  val clTags = Vec(numTags, Bits(Blinded.CL_TAG_SIZE.W))

  /** A bit mask representing which bytes in `bits` are blinded
    * blindmask(0) is for the LSByte and blindmask(n-1) is for the MSByte
    * @group Signals
    */
  // val blindmask = mask.cloneType.asInstanceOf[M]

  /** The data to be transferred
    * @group Signals
    */
  val bits = gen.cloneType.asInstanceOf[T]
}

object BlindedMem {

  val CL_TAG_OFFSET_BITS = 3
  val CL_TAG_GRANULE_SZ = 8 // must be 2^(CL_TAG_OFFSET_BITS)

  /** Wrap some [[Data]] in a blinded interface
    * @tparam T the type of the data to wrap
    * @param gen the data to wrap
    * @return the wrapped input data
    */
  def apply[T <: Data](gen: T, numTags: Int): BlindedMem[T] = new BlindedMem(gen, numTags)
}
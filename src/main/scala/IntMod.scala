import scala.math.BigInt

object IntMod {

  val LongMaxValue: BigInt = Long.MaxValue;
  val U64MaxValue = (2*(LongMaxValue+1))-1;

  val IntMaxValue: BigInt = Int.MaxValue;
  val U32MaxValue = (2*(IntMaxValue+1))-1;

  def apply(r: BigInt, p: BigInt): IntMod = {
    if (r.isValidLong && p.isValidLong && p<U32MaxValue && p>0)  {
      this(r.longValue, p.longValue)
    }  else {
      throw new IllegalArgumentException("IntMod can only be called with arguments (r, p) where -2^64<=r<2^64 and 0<p<2^32)")
    }
  }

  private def apply(r: Long, p: Long): IntMod = {
    var rMod = r % p
    val rLeast: Long = (if (rMod < 0) {
      rMod + p
    } else {
      rMod
    })
    new IntMod(BigInt(rLeast), BigInt(p))
  }
}

case class IntMod private (val r: BigInt, val p: BigInt) {

  assert(r>=0);
  assert(r<p);
  assert(p>0);

  override def toString: String = s"IntMod(${this.r}, ${this.p})"

  def intValue: Int = this.r.intValue;

  def pow(exponent: Long): IntMod = {
    IntMod(r.modPow(BigInt(exponent), p), p)
  }

  def unary_-(): IntMod = IntMod(-r, p)

  def +(other: IntMod): IntMod = {
    if (this.p != other.p) throw new IllegalArgumentException("Only IntMods with the same prime may be added.");
    IntMod( (this.r + other.r) % this.p, p)
  }

  def -(other: IntMod): IntMod = {
    if (this.p != other.p) throw new IllegalArgumentException("Only IntMods with the same prime may be subtracted.");
    IntMod( (this.r - other.r) % this.p, p)
  }

  def *(other: IntMod): IntMod = {
    if (this.p != other.p) throw new IllegalArgumentException("Only IntMods with the same prime may be multiplied.");
    IntMod( (this.r * other.r) % this.p, p)
  }

  def /(other: IntMod): IntMod = {
    if (this.p != other.p) throw new IllegalArgumentException("Only IntMods with the same prime may be divided.");
    IntMod( (this.r * other.r.modInverse(this.p)) % this.p, p)
  }
}





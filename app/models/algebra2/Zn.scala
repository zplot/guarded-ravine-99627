package models.algebra2

case class Zn(n: Int) extends Ring {

  type T1 = Int
  type T2 = IntModN

  def builder(x: Int) = IntModN(x)

  val structureId: String = "{" + "Zn" + n + "}"
  val zero = IntModN(0)
  val one = IntModN(1)
  val finite = true
  val minusOne = IntModN(n - 1)

  object IntModN {
    def apply(k: Int): IntModN = {
      val v: Int = if (k < 0) {
        (math.abs(k) / n + 1) * n + k
      } else {
        k
      }
      new IntModN(v % n)
    }
  }


  class IntModN private(val k: Int) extends RingElement {

    val elementId: String = k.toString
    val isZero: Boolean = k == 0

    def add(other: IntModN) = IntModN((k + other.k) % n)
    def minus(other: IntModN) = IntModN((k - other.k) % n)
    def multiply(other: IntModN) = IntModN((k * other.k) % n)
    def negate: T2 = builder(n - k)



    /** Power of an element
      *
      * Uses repeated squaring algorithm:
      * http://www.algorithmist.com/index.php/Repeated_Squaring
      * val ring = Zn(7)
      * val a = ring.builder(5)
      * val c = a.power(3)
      */
    def power(p: Int): IntModN = p match {
      case 0 => one
      case 1 => this
      case x if x % 2 == 1 => this * (this * this).power((p - 1) / 2)
      case x if x % 2 == 0 => (this * this).power(p / 2)
    }






    override def toString: String = k.toString + " mod " + n.toString

    final override def equals(other: Any): Boolean = {
      val that = other.asInstanceOf[IntModN]
      if (that == null) false
      else this.k == that.k
    }
  }
}


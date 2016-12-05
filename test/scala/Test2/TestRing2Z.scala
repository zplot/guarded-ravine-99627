package models.algebra

object TestRing2Z extends App {

  println("Empezamos")

  object Z extends Ring2 {

    type T1 = Int
    type T2 = ZInteger

    val zero = ZInteger(0)

    override val binop: (ZInteger, ZInteger) => ZInteger = (x, y) => x.add(y)

    override def builder(x: T1): ZInteger = ZInteger(x)

    override val structureId: String = "Z"

    override val identity: ZInteger = ZInteger(0)

    case class ZInteger(value: Int) extends Ring2Element {

      override def add(other: ZInteger): ZInteger = ZInteger(this.value + other.value)

      def inverse: ZInteger = ZInteger(-value)

      override val isZero: Boolean = this.value == 0

      override def negate = ZInteger(-value)

      override val elementId: String = value.toString






    }


  }



  val a: Z.ZInteger = Z.ZInteger(2)
  val b: Z.ZInteger = Z.ZInteger(3)
  val c: Z.ZInteger = a.add(b)

  println(a)
  println(b)
  println(c)

}
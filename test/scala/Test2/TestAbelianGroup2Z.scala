package models.algebra

object TestAbelianGroup2Z extends App {

  println("Empezamos")

  object ZPlus extends AbelianGroup2 {

    type T1 = Int
    type T2 = ZPlusInteger

    override val binop: (ZPlusInteger, ZPlusInteger) => ZPlusInteger = (x, y) => x.add(y)

    override def builder(x: T1): ZPlusInteger = ZPlusInteger(x)

    override val structureId: String = "SG example"

    override val identity: ZPlusInteger = ZPlusInteger(0)
    override val zero: ZPlusInteger = ZPlusInteger(0)



    case class ZPlusInteger(value: Int) extends AbelianGroup2Element {

      override def add(other: ZPlusInteger): ZPlusInteger = ZPlusInteger(this.value + other.value)

      override val isZero: Boolean = this.value == 0

      override def negate = ZPlusInteger(- this.value)

      override val elementId: String = this.value.toString

      override def inverse = ZPlusInteger(- this.value)

    }
  }

  val a: ZPlus.ZPlusInteger = ZPlus.ZPlusInteger(2)
  val b: ZPlus.ZPlusInteger = ZPlus.ZPlusInteger(3)
  val c: ZPlus.ZPlusInteger= a.add(b)

  println(a)
  println(b)
  println(c)

}



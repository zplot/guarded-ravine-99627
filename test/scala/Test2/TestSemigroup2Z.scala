package models.algebra

object TestSemigroup2Z extends App {

  println("Empezamos")

  object Z extends Semigroup2 {

    type T1 = Int
    type T2 = Integer

    override val binop: (Integer, Integer) => Integer = (x, y) => x.add(y)

    override def builder(x: T1): Integer = Integer(x)

    override val structureId: String = "SG example"

    case class Integer(valor: Int) extends Semigroup2Element {

      def add(other: Integer): Integer = Integer(this.valor + other.valor)
    }


  }



  val a: Z.Integer = Z.Integer(2)
  val b: Z.Integer = Z.Integer(3)
  val c: Z.Integer = a.add(b)

  println(a)
  println(b)
  println(c)

}
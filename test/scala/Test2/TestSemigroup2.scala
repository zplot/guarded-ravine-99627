package models.algebra

object TestSemigroup2 extends App {

  println("Empezamos")

  object SG extends Semigroup2 {

    type T1 = Int
    type T2 = SGElement

    override val binop: (SGElement, SGElement) => SGElement = (x, y) => x * y

    override def builder(x: T1): SGElement = SGElement(x)

    override val structureId: String = "SG example"

    case class SGElement(x: Int) extends Semigroup2Element

    val a: SGElement = SGElement(2)
    val b: SGElement = SGElement(2)
    val c: SGElement = a.multiply(b)

  }


}
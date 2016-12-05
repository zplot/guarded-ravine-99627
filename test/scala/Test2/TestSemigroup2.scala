package models.algebra

object TestSemigroup2 extends App {

  println("Empezamos")

  object SG extends Semigroup2 {

    type T1 = Int
    type T2 = SGElement

    override val binop: (SGElement, SGElement) => SGElement = (x, y) => x.suma(y)

    override def builder(x: T1): SGElement = SGElement(x)

    override val structureId: String = "SG example"

    case class SGElement(valor: Int) extends Semigroup2Element {

      def suma(other: SGElement): SGElement = SGElement(this.valor + other.valor)
    }


  }



  val a: SG.SGElement = SG.SGElement(2)
  val b: SG.SGElement = SG.SGElement(3)
  val c: SG.SGElement = a.operation(b)
  val d: SG.SGElement = a.suma(b)

  println(a)
  println(b)
  println(c)
  println(d)










}
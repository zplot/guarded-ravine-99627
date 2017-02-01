package algebra2


import models.algebra2.FiniteGroupExamples._

object TestFiniteGroups extends App {

  println("Empezamos")

  println()

  val grupo = DirectProduct(S(4), Q8)
  println(grupo)

  val grupo2 = S(7)
  println(grupo2)
}
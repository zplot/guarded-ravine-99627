package algebra2


import models.algebra2._


object TestPermutation extends App {





  println("Empezamos")

  val ciclo1: Cycle = Cycle(List(1,2,3,4,5))
  val ciclo2: Cycle = Cycle(List(1,3))
  val a: Set[Cycle] = Set(ciclo1, ciclo2)

  val b: Permutation = Permutation(a)
  println("b = " + b)




}
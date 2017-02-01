package models.algebra2


object TestPermutation extends App {





  println("Empezamos")


  val b = Cycle((1 to 5).toList)

  val c = Cycle(List(1,2))

  val d: Set[Cycle] = Set(b,c)
  val per: Permutation = Permutation(d)
  println("per = " + per)


}
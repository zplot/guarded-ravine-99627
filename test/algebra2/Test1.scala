package models.algebra2


object Test1 extends App {





  println("Empezamos")

  val a = Cycle(List(1,2,3))
  println(a)
  val b = Cycle((1 to 5).toList)
  println(b)
  val c: Set[Cycle] = Set(b)
  println("c= " + c)
  //val d = Prueba1(c)
  val dd: Permutation = Permutation(c)
  println(dd)


}
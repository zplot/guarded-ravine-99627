package models.algebra2




object Test1 extends App {


  import scala.language.implicitConversions





  println("Empezamos")


  val ring1 = Z
  val polyRing = PolynomialOverRing(Z)
  val polyRing2 = PolynomialOverRing(polyRing)
  println(polyRing2)


  // Convierte un Int en un elemento del anillo
  implicit def conv1(x: Int): polyRing.ring.T2 = ring1.builder(x).asInstanceOf[polyRing.ring.T2]







  import Z.conversor1




  val map1: Map[Int, polyRing.ring.T2] = Map(2 -> 3, 3 -> 4, 8 -> 11, 7 -> 2)

  val p1 = polyRing.builder(map1)

  println("p1 = " + p1 * p1)










}
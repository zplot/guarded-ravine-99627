package models.algebra2

/**
  * A monoid with inverse elements
  *
  * https://en.wikipedia.org/wiki/Group_(mathematics)
  */

trait Group extends Monoid {

  type T1
  type T2 <: GroupElement

  def builder(x: T1): T2

  val one: T2

  def commutator(g: T2, h: T2): T2 = {
    g.multiply(h.multiply(g.inverse.multiply(h.inverse)))
  }


  def generatedSet(generators: Set[T2]): Set[T2] = {
    def loop(previousList: Set[T2], generators: Set[T2]): Set[T2] = {
      val newList =
        for {
          g <- previousList
          s <- generators
        } yield g.multiply(s)
      if (previousList != newList) {
        loop(newList ++ previousList, generators)
      } else {
        previousList
      }
    }
    val initialList = generators + one
    val finalList = loop(initialList, generators)
    finalList
  }

  trait GroupElement extends MonoidElement {

    val fatherGroup: Group = Group.this

    def inverse: T2

  }
}


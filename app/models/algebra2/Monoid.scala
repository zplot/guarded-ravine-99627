package models.algebra2

/**
  * A semigroup with an identity element
  *
  * https://en.wikipedia.org/wiki/Monoid
  */

trait Monoid extends Semigroup {

  type T1
  type T2 <: MonoidElement

  def builder(x: T1): T2

  val identity: T2

  trait MonoidElement extends SemigroupElement {

    val fatherMonoid: Monoid = Monoid.this

  }
}

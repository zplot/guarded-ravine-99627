package models.algebra2

/**
  * A semigroup is a magma that satisfies the associative property
  *
  * https://en.wikipedia.org/wiki/Semigroup
  */

trait Semigroup extends Magma {

  type T1
  type T2 <: SemigroupElement

  def builder(x: T1): T2

  trait SemigroupElement extends MagmaElement {

    val fatherSemigroup: Semigroup = Semigroup.this

  }
}

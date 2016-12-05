package models.algebra

trait Monoid2 extends Semigroup2 {

  type T1
  type T2 <: Monoid2Element

  def builder(x: T1): T2

  val structureId: String
  val identity: T2

  override def toString: String = structureId

  trait Monoid2Element extends Semigroup2Element {

    val fatherMonoid: Monoid2 = Monoid2.this

  }
}


package models.algebra

trait Ring2 extends AbelianGroup2 with Semigroup2 {



  type T1
  type T2 <: Ring2Element

  def builder(x: T1): T2

  val structureId: String
  val finite: Boolean
  val one: T2
  val zero: T2

  trait Ring2Element extends AbelianGroup2Element with Semigroup2Element {

    val fatherRing: Ring2 = Ring2.this



  }
}

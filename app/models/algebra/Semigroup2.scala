package models.algebra

import scala.language.implicitConversions

trait Semigroup2 {

  type T1
  type T2 <: Semigroup2Element

  implicit def conv(x: Semigroup2Element): T2 = x.asInstanceOf[T2]


  val binop: (T2, T2) => T2

  def builder(x: T1): T2

  val structureId: String

  override def toString: String = structureId

  trait Semigroup2Element {

    val fatherSemigroup: Semigroup2 = Semigroup2.this

    def operation(other: T2): T2 = binop(this, other)

  }
}

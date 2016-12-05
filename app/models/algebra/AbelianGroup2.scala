package models.algebra

import scala.language.implicitConversions

trait AbelianGroup2 extends Group2 {

  type T1
  type T2 <: AbelianGroup2Element

  implicit def conv(x: AbelianGroup2Element): T2 = x.asInstanceOf[T2]


  val operation: (T2, T2) => T2 = binop

  def builder(x: T1): T2

  val structureId: String
  val zero: T2


  override def toString: String = structureId

  trait AbelianGroup2Element extends Group2Element {

    val fatherAbelianGroup2: AbelianGroup2 = AbelianGroup2.this
    val isZero: Boolean

    def negate: T2

    def add(other: T2): T2 = operation(this, other)



    def +(other: T2): T2 = this.add(other)
    def minus(other: T2): T2 = binop(this, other.negate)
    def -(other: T2): T2 = this.minus(other)




    override def toString: String
  }
}



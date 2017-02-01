package models.algebra2

/**
  * In abstract algebra, a magma is a basic kind of algebraic structure. Specifically, a magma consists
  * of a set, M, equipped with a single binary operation, M × M → M. The binary operation must be closed by
  * definition but no other properties are imposed.
  *
  * https://en.wikipedia.org/wiki/Magma_(algebra)
  */




trait Magma {

  type T1
  type T2 <: MagmaElement

  def builder(x: T1): T2

  val structureId: String

  override def toString: String = "{" + structureId + "}"

  trait MagmaElement {

    val fatherMagma: Magma = Magma.this
    val elementId: String
    def multiply(other: T2): T2
    def *(other: T2): T2 = this.multiply(other)
    override def toString: String = elementId.toString
  }
}



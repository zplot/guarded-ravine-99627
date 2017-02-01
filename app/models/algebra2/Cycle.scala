package models.algebra2

/**
  * Created by LuisFontes on 29/01/2017.
  */


object Cycle {

  private def normalForm(y: Cycle): Cycle = y.cycle.head match {
    case x if x == y.cycle.min => y
    case _ => normalForm(shiftRight(y))
  }

  private def shiftRight(c: Cycle): Cycle = {
    Cycle(c.cycle.last :: c.cycle.dropRight(1))
  }


  private def fromListToMap(list: List[Int]): Map[Int, Int] = {
    list match {
      case Nil => Map[Int, Int]()
      case _ :: Nil => Map[Int, Int]()
      case x :: xs => Map(x -> xs.head) ++ fromListToMap(xs)
    }
  }
}

/** A cycle.
  *
  * @param cycle the cycle as a List[Int]
  *
  * normal: cicle in normal form
  * toList: List[Int] of the cycle in normal form
  * toMap: cycle as a Map[Int, Int]
  */
case class Cycle(cycle: List[Int]) {

  val normal: Cycle = Cycle.normalForm(this)
  val toList: List[Int] = normal.cycle
  val length: Int = cycle.length
  val first: Int = cycle.head
  val toMap: Map[Int, Int] = Cycle.fromListToMap(cycle :+ cycle.head)


  final override def equals(other: Any): Boolean = {
    val that = other.asInstanceOf[Cycle]
    if (that == null) false
    else normal.cycle == that.normal.cycle
  }

  override def toString: String = "(" + normal.cycle.mkString(" ") + ")"

}


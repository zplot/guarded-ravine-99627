package models.algebra2




object Permutation {

  def apply(t: Set[Cycle]): Permutation = {

    val struc: Set[Cycle] = if (t.filter(x => x.cycle.length > 1) == Set[Cycle]()) Set(Cycle(List(1))) else t.filter(x => x.cycle.length > 1)


    new Permutation(struc)

  }

  def unapply(x: Permutation): Option[Set[Cycle]] = Some(x.struc)

  def fromListOfCyclestoMap(lc: List[Cycle]): Map[Int, Int] = {

    lc match {
      case Nil => Map[Int, Int]()
      case x :: xs => x.toMap ++ fromListOfCyclestoMap(xs)

    }
  }

  val one = new Permutation(Set(Cycle(List(1))))

  def generar(generadores: Set[Permutation]): Set[Permutation] = {

    def bucleMultiple(listaAnterior: Set[Permutation], generadores: Set[Permutation]): Set[Permutation] = {

      val listaNueva =
        for {
          g <- listaAnterior
          s <- generadores
        } yield g.multiply(s)

      if (listaAnterior != listaNueva) {
        bucleMultiple(listaNueva ++ listaAnterior, generadores)
      } else {
        listaAnterior
      }
    }
    val listaInicial = generadores + one
    bucleMultiple(listaInicial, generadores)
  }

}

class Permutation(val struc: Set[Cycle]) {

  val numCiclos: Int = struc.size

  //println("struc4 = " + struc.toList.map(x => x.length).max)

  val cicloMasLargo: Int = if (struc == Set()) 0 else struc.toList.map(x => x.length).max



  val toMap: Map[Int, Int] = Permutation.fromListOfCyclestoMap(struc.toList)

  // lowest canonical order
  val lco: List[Cycle] = {

    def orden(cycle1: Cycle, cycle2: Cycle): Cycle = (cycle1.toList, cycle2.toList) match {
      case (List(), List()) => cycle1
      case (_, List()) => cycle1
      case (List(), _) => cycle2
      case (x, y) if x.length < y.length => cycle1
      case (x, y) if x.length > y.length => cycle2
      case (x :: _, y :: _) if x < y => cycle1
      case (x :: _, y :: _) if x > y => cycle2
      case _ => cycle1

    }
    // TODO Esto va mal
    struc.toList.sortWith((cycle1, cycle2) => orden(cycle1, cycle2) == cycle1)
  }

  override def toString: String = if (lco == List()) "1" else "P[" + lco.mkString("") + "]"

  def inver: Permutation = {

    if (cicloMasLargo < 2) Permutation.one
      // El inverso de 1 es 1
      val tmp1 = this.lco
      val tmp2 = tmp1.map(x => x.cycle)
      val tmp3 = tmp2.map(x => x.reverse)
      val tmp4 = tmp3.map(x => Cycle(x))
      val tmp5 = tmp4.toSet
      Permutation(tmp5)
    }

  def multiply(other: Permutation): Permutation = (this, other) match {

    case (Permutation.one, Permutation.one) => Permutation.one
    case (x, Permutation.one) => x
    case (Permutation.one, x) => x
    case (_, _) =>

      val izda: List[Cycle] = this.struc.toList
      val dcha: List[Cycle] = other.struc.toList
      val tmp1: List[Cycle] = izda ++ dcha
      val seMueven: List[Int] = tmp1.flatMap(x => x.cycle).distinct.sorted
      val mapaProducto: Map[Int, Int] =
        seMueven.foldRight[Map[Int, Int]](Map[Int, Int]())((i, mapa) =>
          mapa ++ Map(i -> this.toMap.getOrElse(other.toMap.getOrElse(i, i), other.toMap.getOrElse(i, i))))

      def concatenar(mapa: Map[Int, Int], lista: List[Int]): List[Int] = {
        if (!mapa.contains(lista.last)) {
          lista.dropRight(1)
        } else {
          concatenar(mapa - lista.last, lista :+ mapa(lista.last))
        }
      }

      def extinguir(mapa: Map[Int, Int], quedan: List[Int], ciclos: List[List[Int]]): List[List[Int]] = {

        if (quedan == List()) {
          ciclos
        } else {
          val cicloSiguiente = concatenar(mapa, List(quedan.head))
          val resultado1 = extinguir(mapa, quedan diff ciclos.flatten, ciclos ++ List(cicloSiguiente)).distinct
          resultado1
        }
      }


      val resultado1 = extinguir(mapaProducto, seMueven, List())
      val resultado2 = resultado1.map(x => Cycle(x)).toSet
      Permutation(resultado2)
  }

  override def equals(o: Any): Boolean = o match {

    case that: Permutation => that.lco == this.lco
    case _ => false

  }

  override def hashCode: Int = lco.hashCode

}


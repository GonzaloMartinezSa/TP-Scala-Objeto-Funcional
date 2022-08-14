package tadpquest.funciones

import tadpquest.{Heroe, Stat, Trabajo}

trait Restriccion extends (Heroe => Boolean)

case class StatMayorAStat(stat1: Stat, stat2: Stat) extends Restriccion {
  override def apply(heroe: Heroe): Boolean = heroe.stats.getStat(stat1) > heroe.stats.getStat(stat2)
}

case class RestriccionSobreStatBase(statBase: Stat, valor: Int) extends Restriccion {
  override def apply(heroe: Heroe): Boolean = heroe.statsBase.getStat(statBase) > valor
}

case class RestriccionSobreTrabajoSer(trabajo: Trabajo) extends Restriccion {
  override def apply(heroe: Heroe): Boolean = heroe.trabajo.contains(trabajo)
}

case class RestriccionSobreTrabajoNoSer(trabajo: Trabajo) extends Restriccion {
  override def apply(heroe: Heroe): Boolean = ! RestriccionSobreTrabajoSer(trabajo)(heroe)
}

case object RestriccionSobreTrabajoNoTiene extends Restriccion {
  override def apply(heroe: Heroe): Boolean = heroe.trabajo.isEmpty
}

case class RestriccionesORestricciones(res1: List[Restriccion], res2: List[Restriccion]) extends Restriccion {
  override def apply(heroe: Heroe): Boolean = res1.forall(r => r(heroe)) || res2.forall(r => r(heroe))
}
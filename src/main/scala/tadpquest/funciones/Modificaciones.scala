package tadpquest.funciones

import tadpquest._

/*object Modificaciones {

  type Modificacion = Heroe => Heroe

  // Agregar - Sustraer un valor al original
  val decorarStat:   Stat => Int => Modificacion = stat => valor => heroe => heroe.decorarStat(stat, valor)
  // Cambiar el original por otro valor completamente diferente
  val modificarStat: Stat => Int => Modificacion = stat => valor => heroe => heroe.modificarStat(stat, valor)

}*/

trait Modificacion extends (Heroe => Heroe)

case class DecorarStat(stat: Stat, valor: Int) extends Modificacion {
   override def apply(heroe: Heroe): Heroe = heroe.decorarStat(stat, valor)
}

case class DecorarStatTimes(stat: Stat, valor: Int, func: Heroe => Int) extends Modificacion {
  override def apply(heroe: Heroe): Heroe = heroe.decorarStat(stat, valor * func(heroe))
}

case class ModificarStat(stat: Stat, valor: Int) extends Modificacion {
   override def apply(heroe: Heroe): Heroe = heroe.modificarStat(stat, valor)
}

case class CopiarStat(statDestino: Stat, statOrigen: Stat) extends Modificacion {
   override def apply(heroe: Heroe): Heroe = ModificarStat(statDestino, heroe.stats.getStat(statOrigen))(heroe)
}

case object StatPrincipal extends Criterio {
  override def apply(heroe: Heroe): Int = heroe.trabajo.fold(0)(t => heroe.stats.getStat(t.statPrincipal))
}

case class Porcentaje(porcentaje: Double) extends (Int => Int) {
  override def apply(valor: Int): Int = (porcentaje * valor).floor.toInt
}

case class ModificacionesOModificaciones(mods1: List[Modificacion], mods2: List[Modificacion],
                                         crieterio: Heroe => Boolean) extends Modificacion {

  override def apply(heroe: Heroe): Heroe = if(crieterio(heroe)) {
    mods1.foldLeft(heroe)((result, mod) => mod(result))
  } else {
    mods2.foldLeft(heroe)((result, mod) => mod(result))
  }
}
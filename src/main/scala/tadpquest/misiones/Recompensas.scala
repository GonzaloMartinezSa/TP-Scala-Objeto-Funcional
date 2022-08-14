package tadpquest.misiones

import tadpquest.funciones.Modificacion
import tadpquest.{Equipo, Heroe}

trait Recompensa extends (Equipo => Equipo)

case class AgregarAPozoComun(valor: Int) extends Recompensa {
  override def apply(equipo: Equipo): Equipo = equipo.agregarAPozoComun(valor)
}

case class AgregarNuevoHeroe(heroe: Heroe) extends Recompensa {
  override def apply(equipo: Equipo): Equipo = equipo.obtenerMiembro(heroe)
}

case class ModificacionDeStats(modificaciones: List[Modificacion], condicion: Heroe => Boolean) extends Recompensa {
  override def apply(equipo: Equipo): Equipo = equipo.copy(heroes = equipo.heroes.map(heroe => if(condicion(heroe)) {
    modificaciones.foldLeft(heroe)((result, mod) => mod(result))
  } else heroe)
  )
}
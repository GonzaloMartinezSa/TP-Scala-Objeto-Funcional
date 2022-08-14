package tadpquest.misiones

import tadpquest.funciones.Modificacion
import tadpquest.{Equipo, Fuerza, Guerrero, Heroe, Hp, Item, Ladron, Mago}
import tadpquest.{TryTarea, Exito, Fallo}


class Tarea(modificacion: Modificacion, facilidad: Facilidad, restriccion: RestriccionEquipo = _ => true) {

  def heroeMasApto(equipo: Equipo) :Heroe = equipo.mejorHeroeSegun(heroe => facilidad(equipo, heroe)).get

  def serRealizadaPor(equipo: Equipo) :TryTarea = {
    if (!restriccion(equipo)) return Fallo(equipo, this)

    val heroe = heroeMasApto(equipo)
    val heroeTareaCompletada = modificacion(heroe)
    val equipoActualizado = equipo.reemplazarMiembro(heroe, heroeTareaCompletada)
    Exito(equipoActualizado)
  }

}


trait Facilidad extends ((Equipo, Heroe) => Int)

//----------------------------------------------------------------------------------------------------------

case object EfectoPelearContraMounstruo extends Modificacion {
  override def apply(heroe: Heroe): Heroe = if(heroe.fuerza < 20) {heroe.decorarStat(Hp, -3)} else heroe
}

case object FacilidadPelearContraMounstruo extends Facilidad {
  override def apply(equipo: Equipo, heroe: Heroe): Int = equipo.lider.fold(10)(_.trabajo match {
    case Some(Guerrero) => 20
    case _ => 10
  })
}

case object PelearContraMounstruo extends Tarea(EfectoPelearContraMounstruo, FacilidadPelearContraMounstruo)
//----------------------------------------------------------------------------------------------------------


case object EfectoForzarPuerta extends Modificacion {
  override def apply(heroe: Heroe): Heroe = heroe.trabajo.fold(heroe) {
    case Mago | Ladron => heroe
    case _ => heroe.decorarStat(Fuerza, 1).decorarStat(Hp, -5)
  }
}

case object FacilidadForzarPuerta extends Facilidad {
  override def apply(equipo: Equipo, heroe: Heroe): Int = heroe.inteligencia + 10 *
    equipo.heroes.map(_.trabajo).count(_.contains(Ladron))
}

case object ForzarPuerta extends Tarea(EfectoForzarPuerta, FacilidadForzarPuerta)
//----------------------------------------------------------------------------------------------------------


case class EfectoRobarTalisman(talisman: Item) extends Modificacion {
  // Este es medio rancio por la ambiguedad del enunciado
  override def apply(heroe: Heroe): Heroe = heroe.copy(inventario = heroe.inventario.equiparItem(talisman))
}

case object FacilidadRobarTalisman extends Facilidad {
  override def apply(equipo: Equipo, heroe: Heroe): Int = heroe.velocidad
}

trait RestriccionEquipo extends (Equipo => Boolean)

case object RestriccionRobarTalisman extends RestriccionEquipo {
  override def apply(equipo: Equipo): Boolean = equipo.lider.fold(false)(_.trabajo.contains(Ladron))
}

case class RobarTalisman(talisman: Item) extends Tarea(EfectoRobarTalisman(talisman),
  FacilidadRobarTalisman,
  RestriccionRobarTalisman)
//----------------------------------------------------------------------------------------------------------
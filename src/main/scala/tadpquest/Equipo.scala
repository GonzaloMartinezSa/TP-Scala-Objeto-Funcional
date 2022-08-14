package tadpquest

import tadpquest.funciones.CriterioStatPrincipal
import tadpquest.misiones.Mision


case class Equipo(nombre: String, heroes: List[Heroe], pozoComun: Int) {

  def obtenerMiembro(heroe: Heroe): Equipo = copy(heroes = this.heroes.appended(heroe))

  def reemplazarMiembro(heroeSacar: Heroe, heroeMeter: Heroe): Equipo = copy(
    heroes = heroes.filterNot(_.equals(heroeSacar)).appended(heroeMeter)
  )

  def mejorHeroeSegun(criterio: Heroe => Int) :Option[Heroe] = heroes.maxByOption(criterio)

  def lider :Option[Heroe] = mejorHeroeSegun(CriterioStatPrincipal) match {
    case None => None
    case Some(rta) => if (heroes.count(heroe => CriterioStatPrincipal(heroe) == CriterioStatPrincipal(rta)) > 1) {
      None
    } else {
      Some(rta)
    }
  }

  def agregarAPozoComun(valor: Int): Equipo = copy(pozoComun = pozoComun + valor)

  def realizarMision(mision: Mision) :TryMision = {
    mision.serRealizadaPor(this)
  }

}

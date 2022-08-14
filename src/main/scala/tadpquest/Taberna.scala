package tadpquest

import tadpquest.misiones.Mision
import scala.util.{Failure, Success, Try}

class Taberna(misiones: List[Mision]) {

  def elegirMision(misionesElegir: List[Mision], equipo: Equipo, criterio: (Equipo, Equipo) => Boolean) :Try[Mision] = {

    val misionesOK = for {
      mision <- misionesElegir
      resultado = mision.serRealizadaPor(equipo)
      if(resultado.isSuccess)
    } yield mision

    Try(
      if(misionesOK.isEmpty) throw NoSeEncontroMisionParaHacer
      else misionesOK.sortWith((m1,m2) =>
        criterio(m1.serRealizadaPor(equipo).get,m2.serRealizadaPor(equipo).get)
      ).head
    )

  }

  def entrenar(equipo: Equipo) :Equipo = {
    // Esto se puede mejorar, pero funciona
    var misionesAux = misiones
    var equipoActualizado = equipo
    for(cant <- 0 to misiones.length) {
      var mision = elegirMision(misionesAux, equipoActualizado, CriterioOro)
      if(mision.isSuccess) {
        equipoActualizado = equipoActualizado.realizarMision(mision.get).get
        // Cuidado con este filterNot. Si la lista de misiones tenia mas de una mision que era la misma, explota
        misionesAux = misionesAux.filterNot(_.equals(mision.get))
      } else {
        return equipoActualizado
      }
    }
    equipoActualizado
  }

}

object NoSeEncontroMisionParaHacer extends RuntimeException("No Se Encontro Mision Para Hacer")

case object CriterioOro extends ((Equipo, Equipo) => Boolean) {
  override def apply(e1: Equipo, e2: Equipo): Boolean = e1.pozoComun > e2.pozoComun
}
package tadpquest.misiones

import tadpquest.{Equipo, Exito, Fallo, TryMision, TryTarea}

class Mision(tareas: List[Tarea], recompensa: Recompensa) {

  def serRealizadaPor(equipo: Equipo) :TryMision = tareas.foldLeft(TryTarea(equipo))(
    (resultado, tarea) => resultado.bind(team => tarea.serRealizadaPor(team))
  ) .bind(team => Exito(recompensa(team)))

  /*match {
    case Fallo(equipo, tarea) => Fallo(equipo, tarea)
    case Exito(equipo) => Exito(recompensa(equipo))
  }*/

}


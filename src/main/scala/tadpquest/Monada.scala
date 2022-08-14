package tadpquest

import tadpquest.misiones.Tarea

/*object TryMision extends (TryTarea => TryMision) {
  override def apply(tTarea: TryTarea) :TryTarea =
}*/

object TryTarea extends (Equipo => TryTarea) {
  override def apply(equipo: Equipo) :TryTarea = Exito(equipo)
}

trait TryMision {
  def get :Equipo
  def fold[B](ini :B)(func :(Equipo => B)) :B
  def isFailure :Boolean
  def isSuccess :Boolean

  def bind(func: Equipo => TryMision) :TryMision = {
    this match {
      case Fallo(equipo, tarea) => Fallo(equipo, tarea)
      case Exito(equipo) => func(equipo)
    }
  }

}

trait TryTarea extends TryMision {

  def bind(func: Equipo => TryTarea) :TryTarea = {
    this match {
      case Fallo(equipo, tarea) => Fallo(equipo, tarea)
      case Exito(equipo) => func(equipo)
    }
  }

}

case class Fallo(equipo: Equipo, tarea: Tarea) extends TryMision with TryTarea {
  override def get :Equipo = throw FalloNoPuedeHacerGet
  override def fold[B](ini :B)(func :(Equipo => B)) :B = ini
  override def isFailure: Boolean = true
  override def isSuccess: Boolean = false
}

case class Exito(equipo: Equipo) extends TryMision with TryTarea {
  override def get: Equipo = equipo
  override def fold[B](ini :B)(func :(Equipo => B)) :B = func(equipo)
  override def isFailure: Boolean = false
  override def isSuccess: Boolean = true
}

object FalloNoPuedeHacerGet extends RuntimeException
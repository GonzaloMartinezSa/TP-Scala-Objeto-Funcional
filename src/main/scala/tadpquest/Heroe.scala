package tadpquest

import scala.util.{Failure, Success, Try}

case class Heroe(statsBase: Stats, stats: Stats, trabajo: Option[Trabajo], inventario: Inventario) {

  lazy val statsFinales: Stats = modificacionPorItem(modificacionPorTrabajo(this)).stats

  def getStat(stat: Stat) :Int = statsFinales.getStat(stat)

  lazy val hp :Int = statsFinales.hp
  lazy val fuerza :Int = statsFinales.fuerza
  lazy val velocidad :Int = statsFinales.velocidad
  lazy val inteligencia :Int = statsFinales.inteligencia

  def decorarStat(stat: Stat, valor: Int): Heroe = copy(stats = this.stats.decorarStat(stat, valor))
  def modificarStat(stat: Stat, valor: Int): Heroe = copy(stats = this.stats.modificarStat(stat, valor))


  def asignarTrabajo(nuevoTrabajo: Option[Trabajo]): Heroe = copy(trabajo = nuevoTrabajo)

  def equiparItem(item: Item) :Try[Heroe] = if (!item.cumpleConRestricciones(this)) {
    Failure(NoCumpleConRestricciones("El heroe trato de equiparse con un item para el que no cumplia las restricciones"))
  } else {
    Success(copy(inventario = inventario.equiparItem(item)))
  }

  def modificacionPorTrabajo(heroe: Heroe): Heroe = trabajo.fold(heroe)(_.aplicarModificaciones(heroe))
  def modificacionPorItem(heroe: Heroe): Heroe    = inventario.aplicarModificaciones(heroe)

}

case class NoCumpleConRestricciones(msg: String) extends RuntimeException(msg)

trait Stat
case object Hp extends Stat
case object Fuerza extends Stat
case object Velocidad extends Stat
case object Inteligencia extends Stat

case class Stats(hp: Int, fuerza: Int, velocidad: Int, inteligencia: Int) {

  def getStat(stat: Stat) :Int = stat match {
    case Hp           => hp
    case Fuerza       => fuerza
    case Velocidad    => velocidad
    case Inteligencia => inteligencia
  }

  def decorarStat(stat: Stat, valor: Int): Stats = stat match {
    case Hp               => copy(hp = (hp + valor).max(1))
    case Fuerza           => copy(fuerza = (fuerza + valor).max(1))
    case Velocidad        => copy(velocidad = (velocidad + valor).max(1))
    case Inteligencia     => copy(inteligencia = (inteligencia + valor).max(1))
  }

  def modificarStat(stat: Stat, valor: Int): Stats = stat match {
    case Hp               => copy(hp = valor.max(1))
    case Fuerza           => copy(fuerza = valor.max(1))
    case Velocidad        => copy(velocidad = valor.max(1))
    case Inteligencia     => copy(inteligencia = valor.max(1))
  }

}

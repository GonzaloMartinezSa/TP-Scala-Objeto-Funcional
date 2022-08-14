package tadpquest

import tadpquest.funciones.{DecorarStat, Modificacion}

class Trabajo(modificaciones: List[Modificacion], val statPrincipal: Stat) {

  def aplicarModificaciones(heroe: Heroe): Heroe =
    modificaciones.foldLeft(heroe)((result, mod) => mod(result))

}

case object Guerrero extends Trabajo(
  List(
    DecorarStat(Hp, 10),
    DecorarStat(Fuerza,15),
    DecorarStat(Inteligencia, -10)),
  Fuerza
)

case object Mago extends Trabajo(
  List(
    DecorarStat(Inteligencia, 20),
    DecorarStat(Fuerza, -20)),
  Inteligencia
)

case object Ladron extends Trabajo(
  List(
    DecorarStat(Velocidad, 10),
    DecorarStat(Hp, -5)),
  Velocidad
)

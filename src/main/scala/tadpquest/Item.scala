package tadpquest

import tadpquest.funciones.{CopiarStat, DecorarStat, DecorarStatTimes, Modificacion, ModificacionesOModificaciones, ModificarStat, Porcentaje, Restriccion, RestriccionSobreStatBase, RestriccionSobreTrabajoNoSer, RestriccionSobreTrabajoNoTiene, RestriccionSobreTrabajoSer, RestriccionesORestricciones, StatMayorAStat, StatPrincipal, TamanoInventario}

class Item(val pos: PosicionItem, modificaciones: List[Modificacion], restricciones: List[Restriccion] = List()) {

  def cumpleConRestricciones(heroe: Heroe) :Boolean = restricciones.forall(r => r(heroe))
  def aplicarModificaciones(heroe: Heroe): Heroe =
    modificaciones.foldLeft(heroe)((result, mod) => mod(result))

}

object CascoVikingo
  extends Item(
    Cabeza,
    List(DecorarStat(Hp, 10)),
    List(RestriccionSobreStatBase(Fuerza, 30))
  )

object PalitoMagico
  extends Item(
    Mano,
    List(DecorarStat(Inteligencia, 20)),
    List(
      RestriccionesORestricciones(
        List(RestriccionSobreTrabajoSer(Mago)),
        List(RestriccionSobreTrabajoSer(Ladron), RestriccionSobreStatBase(Inteligencia, 30))
      )
    )
  )

object ArmaduraEleganteSport
  extends Item(
    Torso,
    List(
      DecorarStat(Velocidad, 30),
      DecorarStat(Hp, -30)
    )
  )

object ArcoViejo
  extends Item(
    DosManos,
    List(DecorarStat(Fuerza, 2))
  )

object EscudoAntiRobo
  extends Item(
    Mano,
    List(DecorarStat(Hp, 20)),
    List(RestriccionSobreTrabajoNoSer(Ladron), RestriccionSobreStatBase(Fuerza, 20))
  )

object TalismanDeDedicacion
  extends Item(
    Talismanes,
    List(DecorarStatTimes(Hp, 1, StatPrincipal.andThen(Porcentaje(0.1))))
  )

object TalismanDelMinimalismo
  extends Item(
    Talismanes,
    List(
      DecorarStat(Hp, 50),
      DecorarStatTimes(Hp, -10 , TamanoInventario)
    )
  )

object VinchaDelBufaloDeAgua
  extends Item(
    Cabeza,
    List(
      ModificacionesOModificaciones(
        List(DecorarStat(Inteligencia, 30)),
        List(
          DecorarStat(Hp, 10),
          DecorarStat(Fuerza, 10),
          DecorarStat(Velocidad, 10)
        ),
        StatMayorAStat(Fuerza, Inteligencia)
      ),
    ),
    List(RestriccionSobreTrabajoNoTiene)
  )

object TalismanMaldito
  extends Item(
    Talismanes,
    List(
      ModificarStat(Hp, 1),
      ModificarStat(Fuerza, 1),
      ModificarStat(Velocidad, 1),
      ModificarStat(Inteligencia, 1)
    )
  )

object EspadaDeLaVida
  extends Item(
    Mano,
    List(CopiarStat(Fuerza, Hp))
  )

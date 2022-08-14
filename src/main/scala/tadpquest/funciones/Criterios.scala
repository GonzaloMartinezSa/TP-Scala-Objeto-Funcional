package tadpquest.funciones

import tadpquest.Heroe

trait Criterio extends (Heroe => Int)

case object CriterioStatPrincipal extends Criterio {
  override def apply(heroe: Heroe): Int = heroe.trabajo.fold(0)(t => heroe.getStat(t.statPrincipal))
}

case object TamanoInventario extends Criterio {
  override def apply(heroe: Heroe): Int = heroe.inventario.size()
}

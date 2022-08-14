package tadpquest

case class Inventario(items :List[Item]) {

  def size :Int           = items.size
  def armasUnaMano :Int   = items.count(_.pos.equals(Mano))

  def equiparItem(item: Item) :Inventario = item.pos match {
    case Mano       =>  if (armasUnaMano < 2) agregarItem(item)
                        else reemplazarMano(item)
    case DosManos   => filtarPosYAgregarItem(item).filtrarPos(Mano)
    case Talismanes => agregarItem(item)
    case _          => filtarPosYAgregarItem(item)
  }

  def agregarItem(item: Item)                 :Inventario = copy(items = items.appended(item))
  def filtrarPos(posicionItem: PosicionItem)  :Inventario = copy(items = items.filterNot(_.pos.equals(posicionItem)))
  def filtrarItem(item: Item)                 :Inventario = copy(items = items.filterNot(_.equals(item)))
  def filtarPosYAgregarItem(item: Item)       :Inventario = filtrarPos(item.pos).agregarItem(item)

  def reemplazarMano(item: Item) :Inventario = filtrarItem(items.find(_.pos.equals(Mano)).get).agregarItem(item)


  def aplicarModificaciones(heroe: Heroe): Heroe =
    items.foldLeft(heroe)((result, item) => item.aplicarModificaciones(result))

}

trait PosicionItem
case object Cabeza      extends PosicionItem
case object Torso       extends PosicionItem
case object Mano        extends PosicionItem
case object DosManos    extends PosicionItem
case object Talismanes  extends PosicionItem


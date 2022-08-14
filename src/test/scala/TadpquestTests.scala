import org.scalatest.matchers.should.Matchers._
import org.scalatest.freespec.AnyFreeSpec
import tadpquest.funciones.{RestriccionSobreStatBase, RestriccionSobreTrabajoNoSer, RestriccionSobreTrabajoNoTiene, RestriccionSobreTrabajoSer, RestriccionesORestricciones}
import tadpquest.misiones.{AgregarAPozoComun, ForzarPuerta, Mision, PelearContraMounstruo, RobarTalisman}
import tadpquest.{ArcoViejo, ArmaduraEleganteSport, CascoVikingo, CriterioOro, Equipo, EscudoAntiRobo, EspadaDeLaVida, Fuerza, Guerrero, Heroe, Hp, Inteligencia, Inventario, Ladron, Mago, PalitoMagico, Stats, Taberna, TalismanDeDedicacion, TalismanDelMinimalismo, Velocidad, VinchaDelBufaloDeAgua, funciones}


class TadpquestTests extends AnyFreeSpec {

  "TADP Quest" - {

    val statsDefault = new Stats(10,5,6,3)
    val inventarioVacio = Inventario(List())
    val heroeDefault = Heroe(statsDefault, statsDefault, None, inventarioVacio)

    "Se pueden modificar stats correctamente" - {

      "si quiero decorar al HP, me devuelve un heroe con el HP modificado" in {
        val heroeModificado = heroeDefault.decorarStat(Hp, 5)
        heroeModificado.hp           shouldBe   heroeDefault.hp + 5
        heroeModificado.fuerza       shouldBe   heroeDefault.fuerza
        heroeModificado.velocidad    shouldBe   heroeDefault.velocidad
        heroeModificado.inteligencia shouldBe   heroeDefault.inteligencia
      }

      "si quiero decorar a la Fuerza, me devuelve un heroe con la Fuerza modificada" in {
        val heroeModificado = heroeDefault.decorarStat(Fuerza, 6)
        heroeModificado.hp           shouldBe   heroeDefault.hp
        heroeModificado.fuerza       shouldBe   heroeDefault.fuerza + 6
        heroeModificado.velocidad    shouldBe   heroeDefault.velocidad
        heroeModificado.inteligencia shouldBe   heroeDefault.inteligencia
      }

      "si quiero decorar a la Velocidad, me devuelve un heroe con la Velocidad modificada" in {
        val heroeModificado = heroeDefault.decorarStat(Velocidad, -3)
        heroeModificado.hp           shouldBe   heroeDefault.hp
        heroeModificado.fuerza       shouldBe   heroeDefault.fuerza
        heroeModificado.velocidad    shouldBe   (heroeDefault.velocidad - 3).max(1)
        heroeModificado.inteligencia shouldBe   heroeDefault.inteligencia
      }

      "si quiero decorar a la Inteligencia, me devuelve un heroe con la Inteligencia modificada" in {
        val heroeModificado = heroeDefault.decorarStat(Inteligencia, -8)
        heroeModificado.hp           shouldBe   heroeDefault.hp
        heroeModificado.fuerza       shouldBe   heroeDefault.fuerza
        heroeModificado.velocidad    shouldBe   heroeDefault.velocidad
        heroeModificado.inteligencia shouldBe   (heroeDefault.inteligencia - 8).max(1)
      }

      "si quiero modificar directamente al Hp, me devuelve un heroe con el Hp modificado" in {
        val heroeModificado = heroeDefault.modificarStat(Hp, 6)
        heroeModificado.hp           shouldBe   6
        heroeModificado.fuerza       shouldBe   heroeDefault.fuerza
        heroeModificado.velocidad    shouldBe   heroeDefault.velocidad
        heroeModificado.inteligencia shouldBe   heroeDefault.inteligencia
      }
    }

    "El trabajo afecta a los stats correctamente" - {

      "si quiero ver como afecta el Trabajo a los stats y no hay un Trabajo" in {
        val heroeModificado = heroeDefault.modificacionPorTrabajo(heroeDefault)
        heroeModificado.hp            shouldBe  heroeDefault.hp
        heroeModificado.fuerza        shouldBe  heroeDefault.fuerza
        heroeModificado.velocidad     shouldBe  heroeDefault.velocidad
        heroeModificado.inteligencia  shouldBe  heroeDefault.inteligencia
      }

      "si quiero ver como afecta a los stats que el Trabajo sea un Guerrero" in {
        val heroeModificado = heroeDefault.asignarTrabajo(Some(Ladron))
        heroeModificado.hp            shouldBe  (heroeDefault.hp-5).max(1)
        heroeModificado.fuerza        shouldBe  heroeDefault.fuerza
        heroeModificado.velocidad     shouldBe  heroeDefault.velocidad+10
        heroeModificado.inteligencia  shouldBe  heroeDefault.inteligencia
      }

      "si quiero ver como afecta a los stats que el Trabajo sea un Mago" in {
        val heroeModificado = heroeDefault.asignarTrabajo(Some(Ladron))
        heroeModificado.hp            shouldBe  (heroeDefault.hp-5).max(1)
        heroeModificado.fuerza        shouldBe  heroeDefault.fuerza
        heroeModificado.velocidad     shouldBe  heroeDefault.velocidad+10
        heroeModificado.inteligencia  shouldBe  heroeDefault.inteligencia
      }

      "si quiero ver como afecta a los stats que el Trabajo sea un Ladron" in {
        val heroeModificado = heroeDefault.asignarTrabajo(Some(Ladron))
        heroeModificado.hp            shouldBe  (heroeDefault.hp-5).max(1)
        heroeModificado.fuerza        shouldBe  heroeDefault.fuerza
        heroeModificado.velocidad     shouldBe  heroeDefault.velocidad+10
        heroeModificado.inteligencia  shouldBe  heroeDefault.inteligencia
      }

    }

    "Las restricciones funcionan bien" - {

      "Si la restriccion es que un heroe tenga minimo X de cierto stat base, un heroe con mas de X en ese stat base la pasa" in {
        val heroeModificado = heroeDefault.copy(statsBase = new Stats(0, 40, 0, 0))
        RestriccionSobreStatBase(Fuerza, 30)(heroeModificado)  shouldBe  true
      }

      "Si la restriccion es que un heroe tenga minimo X de cierto stat base, un heroe con menos de X en ese stat base falla" in {
        val heroeModificado = heroeDefault.copy(statsBase = new Stats(0, 40, 0, 0))
        funciones.RestriccionSobreStatBase(Fuerza, 50)(heroeModificado)  shouldBe  false
      }

      "Si la restriccion es que un heroe no tenga trabajo, un heroe sin trabajo la pasa" in {
        RestriccionSobreTrabajoNoTiene(heroeDefault)  shouldBe  true
      }

      "Si la restriccion es que un heroe no tenga trabajo, un heroe con trabajo falla" in {
        val heroeModificado = heroeDefault.asignarTrabajo(Some(Mago))
        funciones.RestriccionSobreTrabajoNoTiene(heroeModificado)  shouldBe  false
      }

      "Si la restriccion es que el trabajo sea X, un heroe con el trabajo X la pasa" in {
        val heroeModificado = heroeDefault.asignarTrabajo(Some(Mago))
        RestriccionSobreTrabajoSer(Mago)(heroeModificado)  shouldBe  true
      }

      "Si la restriccion es que el trabajo sea X, un heroe con trabajo pero diferente a X falla" in {
        val heroeModificado = heroeDefault.asignarTrabajo(Some(Ladron))
        funciones.RestriccionSobreTrabajoSer(Mago)(heroeModificado)  shouldBe  false
      }

      "Si la restriccion es que el trabajo no sea X, un heroe sin el trabajo X la pasa" in {
        val heroeModificado = heroeDefault.asignarTrabajo(Some(Mago))
        RestriccionSobreTrabajoNoSer(Ladron)(heroeModificado)  shouldBe  true
      }

      "Si la restriccion es que el trabajo no sea X, un heroe con trabajo X falla" in {
        val heroeModificado = heroeDefault.asignarTrabajo(Some(Ladron))
        funciones.RestriccionSobreTrabajoNoSer(Ladron)(heroeModificado)  shouldBe  false
      }

      "Si tiene que cumplir unas restricciones U otras, y cumple las primeras, pasa" in {
        val res1  = funciones.RestriccionSobreTrabajoSer(Mago)
        val res21 = funciones.RestriccionSobreTrabajoSer(Ladron)
        val res22 = funciones.RestriccionSobreStatBase(Inteligencia, 30)
        val heroeModificado = heroeDefault.asignarTrabajo(Some(Mago))
        RestriccionesORestricciones(List(res1), List(res21, res22))(heroeModificado) shouldBe  true
        res1(heroeModificado)   shouldBe  true
        res21(heroeModificado)  shouldBe  false
        res22(heroeModificado)  shouldBe  false
      }

      "Si tiene que cumplir unas restricciones U otras, y cumple las segundas, pasa" in {
        val res1  = funciones.RestriccionSobreTrabajoSer(Mago)
        val res21 = funciones.RestriccionSobreTrabajoSer(Ladron)
        val res22 = funciones.RestriccionSobreStatBase(Inteligencia, 30)
        val heroeModificado = heroeDefault.copy(statsBase = Stats(0,0,0,40)).asignarTrabajo(Some(Ladron))
        RestriccionesORestricciones(List(res1), List(res21, res22))(heroeModificado) shouldBe  true
        res1(heroeModificado)   shouldBe  false
        res21(heroeModificado)  shouldBe  true
        res22(heroeModificado)  shouldBe  true
      }

      "Si tiene que cumplir unas restricciones U otras, y no cumple todas las primeras o segundas, falla" in {
        val res1  = funciones.RestriccionSobreTrabajoSer(Mago)
        val res21 = funciones.RestriccionSobreTrabajoSer(Ladron)
        val res22 = funciones.RestriccionSobreStatBase(Inteligencia, 30)
        val heroeModificado = heroeDefault.copy(statsBase = Stats(0,0,0,20)).asignarTrabajo(Some(Ladron))
        RestriccionesORestricciones(List(res1), List(res21, res22))(heroeModificado) shouldBe  false
        res1(heroeModificado)   shouldBe  false
        res21(heroeModificado)  shouldBe  true
        res22(heroeModificado)  shouldBe  false
      }

    }

    val inventarioDefault = Inventario(List(ArmaduraEleganteSport))

    "El tamano del Inventario se calcula bien" - {

      "Si no hay nada en el inventario, el tamano es 0" in {
        heroeDefault.inventario.size  shouldBe  0
      }

      "Si hay solo una cosa en el inventario, el tamano es 1" in {
        val heroeModificado = heroeDefault.copy(inventario = inventarioDefault)
        heroeModificado.inventario.size  shouldBe  1
      }

      "Si hay multiples cosas en el inventario, el tamano es la cant" in {
        val nuevoInventario = Inventario(List(TalismanDelMinimalismo, TalismanDeDedicacion))
        val heroeModificado = heroeDefault.copy(inventario = nuevoInventario)
        heroeModificado.inventario.size  shouldBe  2
      }

      "Si hay multiples cosas en el inventario, el tamano es la cant (en diferentes partes del cuerpo)" in {
        val nuevoInventario = Inventario(List(CascoVikingo, PalitoMagico, TalismanDeDedicacion))
        val heroeModificado = heroeDefault.copy(inventario = nuevoInventario)
        heroeModificado.inventario.size  shouldBe  3
      }

      "Un arma a una manos cuenta una vez, y la cantidad es la esperada" in {
        val heroeModificado = heroeDefault.copy(statsBase = Stats(5,30,15,15))
          .equiparItem(EspadaDeLaVida).get.equiparItem(EscudoAntiRobo).get
        heroeModificado.inventario.size  shouldBe  2
      }

      "Un arma a dos manos cuenta una vez, aunque use las dos manos" in {
        val heroeModificado = heroeDefault.equiparItem(ArcoViejo).get
        heroeModificado.inventario.size  shouldBe  1
      }

      "Equipar un item en una posicion ya usada pisa al otro (menos en armas de una mano)" in {
        val heroeModificado = heroeDefault.copy(statsBase = Stats(5,40,15,15))
          .equiparItem(CascoVikingo).get.equiparItem(VinchaDelBufaloDeAgua).get
        heroeModificado.inventario.size  shouldBe  1
      }

      "Equipar un item en una posicion de mano funciona correctamente" in {
        val heroeModificado = heroeDefault.copy(statsBase = Stats(5,30,15,15))
          .equiparItem(EspadaDeLaVida).get.equiparItem(EscudoAntiRobo).get
          .equiparItem(EspadaDeLaVida).get
        heroeModificado.inventario.size  shouldBe  2
      }
    }

    "Los items afectan a los stats correctamente" - {

      "Si un heroe no pasa las restricciones de un item e intenta equiparselo, explota todo" in {
        val heroeModificado = heroeDefault.copy(statsBase = Stats(0, 20, 0, 0))
        heroeModificado.equiparItem(CascoVikingo).isFailure shouldBe  true
      }

      "Si un heroe pasa las restricciones de un item e intenta equiparselo, lo puede hacer" in {
        val heroeModificado = heroeDefault.copy(statsBase = Stats(0, 40, 0, 0))
        heroeModificado.equiparItem(CascoVikingo).get.inventario.items.contains(CascoVikingo) shouldBe  true
      }

      "Si me pongo un item Armadura, los stats se ven afectados adecuadamente" in {
        val heroeModificado = heroeDefault.equiparItem(ArmaduraEleganteSport).get
        heroeModificado.hp            shouldBe  (heroeDefault.hp-30).max(1)
        heroeModificado.fuerza        shouldBe  heroeDefault.fuerza
        heroeModificado.velocidad     shouldBe  heroeDefault.velocidad+30
        heroeModificado.inteligencia  shouldBe  heroeDefault.inteligencia
      }

      "Si me pongo un item Arma, los stats se ven afectados adecuadamente" in {
        val heroeModificado = heroeDefault.equiparItem(EspadaDeLaVida).get
        heroeModificado.hp            shouldBe  heroeDefault.hp
        heroeModificado.fuerza        shouldBe  heroeDefault.hp
        heroeModificado.velocidad     shouldBe  heroeDefault.velocidad
        heroeModificado.inteligencia  shouldBe  heroeDefault.inteligencia
      }

      "Si me pongo un item Arma difernete en cada mano, los stats se ven afectados adecuadamente" in {
        val nuevosStats     = Stats(10, 30, 5, 6)
        val heroeDefault3   = Heroe(nuevosStats, nuevosStats, None, inventarioVacio)
        val heroeModificado = heroeDefault3.equiparItem(EspadaDeLaVida).get.equiparItem(EscudoAntiRobo).get
        heroeModificado.hp            shouldBe  heroeDefault3.hp+20
        heroeModificado.fuerza        shouldBe  heroeDefault3.hp
        heroeModificado.velocidad     shouldBe  heroeDefault3.velocidad
        heroeModificado.inteligencia  shouldBe  heroeDefault3.inteligencia
      }

      "Si me pongo un item Arma a dos manos, los stats se ven afectados adecuadamente" in {
        val heroeModificado = heroeDefault.equiparItem(ArcoViejo).get
        heroeModificado.hp            shouldBe  heroeDefault.hp
        heroeModificado.fuerza        shouldBe  heroeDefault.fuerza+2
        heroeModificado.velocidad     shouldBe  heroeDefault.velocidad
        heroeModificado.inteligencia  shouldBe  heroeDefault.inteligencia
      }

      "Si me pongo un item Talisman, los stats se ven afectados adecuadamente" in {
        val heroeModificado = heroeDefault.equiparItem(TalismanDelMinimalismo).get
        heroeModificado.hp            shouldBe  ((heroeDefault.hp+50) - 10 * heroeModificado.inventario.size).max(1)
        heroeModificado.fuerza        shouldBe  heroeDefault.fuerza
        heroeModificado.velocidad     shouldBe  heroeDefault.velocidad
        heroeModificado.inteligencia  shouldBe  heroeDefault.inteligencia
      }

      "Si me pongo dos items, los stats se ven afectados adecuadamente" in {
        val heroeModificado = heroeDefault.equiparItem(EspadaDeLaVida).get.equiparItem(ArmaduraEleganteSport).get
        heroeModificado.hp            shouldBe  (heroeDefault.hp-30).max(1)
        heroeModificado.fuerza        shouldBe  heroeDefault.hp
        heroeModificado.velocidad     shouldBe  heroeDefault.velocidad+30
        heroeModificado.inteligencia  shouldBe  heroeDefault.inteligencia
      }

      "Si me pongo dos items + un trabajo, los stats se ven afectados adecuadamente" in {
        val heroeModificado = heroeDefault.asignarTrabajo(Some(Mago)).
          equiparItem(EspadaDeLaVida).get.equiparItem(ArmaduraEleganteSport).get
        heroeModificado.hp            shouldBe  (heroeDefault.hp-30).max(1)
        heroeModificado.fuerza        shouldBe  heroeDefault.hp
        heroeModificado.velocidad     shouldBe  heroeDefault.velocidad+30
        heroeModificado.inteligencia  shouldBe  heroeDefault.inteligencia+20
      }

    }

    val heroeDefault2 = Heroe(statsDefault, statsDefault, Some(Guerrero), inventarioDefault)
    val equipoDefault = Equipo("La Banda", List(heroeDefault), 0)

    "Los equipos funcionan correctamente" - {

      "Puedo anadir un heroe al equipo" in {
        val equipoModificado = equipoDefault.obtenerMiembro(heroeDefault2)
        equipoModificado.heroes.contains(heroeDefault)  shouldBe  true
        equipoModificado.heroes.contains(heroeDefault2) shouldBe  true
      }

      "Puedo reemplazar un heroe del equipo por otro" in {
        val equipoModificado = equipoDefault.reemplazarMiembro(heroeDefault, heroeDefault2)
        equipoModificado.heroes.contains(heroeDefault)  shouldBe  false
        equipoModificado.heroes.contains(heroeDefault2) shouldBe  true
      }

      "El lider es el que tiene mas en su stat principal, y hay un lider si no son iguales" in {
        val equipoCompleto = equipoDefault.obtenerMiembro(heroeDefault2)
        val lider = equipoCompleto.lider
        lider.isEmpty shouldBe false
        lider.get     shouldBe  heroeDefault2
      }

      "No hay lider si no hay un stat principal de uno que supere a los otros entre los miembros del equipo" in {
        var equipoCompleto = equipoDefault.obtenerMiembro(heroeDefault2)
        equipoCompleto = equipoCompleto.reemplazarMiembro(heroeDefault, heroeDefault2)
        val lider = equipoCompleto.lider
        lider.isEmpty shouldBe true
      }

      "Las tareas funcionan bien" - {

        "El heroe mas apto se calcula bien" in {
          val equipoCompleto = equipoDefault.obtenerMiembro(heroeDefault2)
          ForzarPuerta.heroeMasApto(equipoCompleto) shouldBe  heroeDefault
        }

        "La tarea no puede ser realizada si no se cumple la restriccion" in {
          val equipoCompleto = equipoDefault.obtenerMiembro(heroeDefault2)
          // La restriccion es que el lider sea un ladron
          RobarTalisman(TalismanDelMinimalismo).serRealizadaPor(equipoCompleto).isFailure shouldBe  true
        }

        "La tarea puede ser realizada si se cumple la restriccion" in {
          val heroeDefault3 = Heroe(statsDefault, statsDefault, Some(Ladron), inventarioDefault)
          val equipoCompleto = equipoDefault.obtenerMiembro(heroeDefault3)
          // La restriccion es que el lider sea un ladron
          RobarTalisman(TalismanDelMinimalismo).serRealizadaPor(equipoCompleto).isSuccess shouldBe  true
        }

        "Realizar una tarea actualiza los stats del heroe que la cumplio" in {
          val nuevosStatsDefault = Stats(10, 5, 7, 40)
          val heroeDefault3 = Heroe(nuevosStatsDefault, nuevosStatsDefault, Some(Guerrero), inventarioDefault)
          val equipoCompleto = equipoDefault.obtenerMiembro(heroeDefault3)
          val equipoModificado = ForzarPuerta.serRealizadaPor(equipoCompleto).get
          val heroe3Modificado = equipoModificado.heroes.tail.head

          equipoModificado.heroes.contains(heroeDefault3) shouldBe  false
          heroe3Modificado.hp           shouldBe  (heroeDefault3.hp - 5).max(1)
          heroe3Modificado.fuerza       shouldBe  heroeDefault3.fuerza + 1
          heroe3Modificado.velocidad    shouldBe  heroeDefault3.velocidad
          heroe3Modificado.inteligencia shouldBe  heroeDefault3.inteligencia
        }

      }

      val misionDefault = new Mision(List(PelearContraMounstruo, ForzarPuerta), AgregarAPozoComun(100))
      val misionRestriccion = new Mision(List(RobarTalisman(TalismanDelMinimalismo)), AgregarAPozoComun(100))

      "Las misiones funcionan bien" - {

        "Un equipo puede realizar una mision para la que cumple las restricciones" in {
          val equipoActualizado = equipoDefault.realizarMision(misionDefault)
          equipoActualizado.isSuccess                 shouldBe  true
          equipoActualizado.get.equals(equipoDefault) shouldBe  false
          equipoActualizado.get.pozoComun             shouldBe  equipoDefault.pozoComun + 100
        }

        "Un equipo no puede realizar una mision, si no cumple las restricciones de una de las tareas" in {
          val equipoActualizado = equipoDefault.realizarMision(misionRestriccion)
          equipoActualizado.isFailure shouldBe  true
        }

        "Al cumplir una mision, los heroes del equipo se ven afectados correctamente" in {
          val nuevosStatsDefault = Stats(10, 5, 7, 40)
          val nuevoInventario = Inventario(List())
          val heroeDefault3 = Heroe(nuevosStatsDefault, nuevosStatsDefault, Some(Guerrero), nuevoInventario)
          val equipoActualizado = equipoDefault.obtenerMiembro(heroeDefault3).realizarMision(misionDefault)
          val heroe1 = equipoActualizado.get.heroes.head
          val heroe2 = equipoActualizado.get.heroes.tail.head

          heroe1.hp shouldBe  (heroeDefault.hp - 3).max(1)
          heroe1.fuerza shouldBe  heroeDefault.fuerza
          heroe1.velocidad shouldBe  heroeDefault.velocidad
          heroe1.inteligencia shouldBe  heroeDefault.inteligencia

          heroe2.hp shouldBe  (heroeDefault3.hp - 5).max(1)
          heroe2.fuerza shouldBe  heroeDefault3.fuerza + 1
          heroe2.velocidad shouldBe  heroeDefault3.velocidad
          heroe2.inteligencia shouldBe  heroeDefault3.inteligencia
        }

      }

      val misionDefault2 = new Mision(List(PelearContraMounstruo, ForzarPuerta), AgregarAPozoComun(100))
      val tabernaFail = new Taberna(List(misionRestriccion, misionRestriccion))
      val tabernaMid  = new Taberna(List(misionRestriccion, misionDefault))
      val taberna     = new Taberna(List(misionDefault, misionDefault2))

      "La Taberna." - {

        "Si trata de elegir una mision, pero no hay ninguna para hacer, falla" in {
          tabernaFail.elegirMision(List(misionRestriccion, misionRestriccion), equipoDefault, CriterioOro).isFailure shouldBe  true
        }

        "Si trata de elegir una mision, y hay alguna para hacer, funca" in {
          tabernaMid.elegirMision(List(misionRestriccion, misionDefault), equipoDefault, CriterioOro).isSuccess shouldBe  true
        }

        "Si no podia hacer ninguna mision, devuelve el mismo equipo" in {
          val equipoActualizado = tabernaFail.entrenar(equipoDefault)
          equipoActualizado.equals(equipoDefault) shouldBe  true
          equipoActualizado.pozoComun             shouldBe  equipoDefault.pozoComun
        }

        "Si podia hacer algunas misiones y otras no, devuelve un equipo diferente, que se ve afectado correctamente" in {
          val equipoActualizado = tabernaMid.entrenar(equipoDefault)
          equipoActualizado.equals(equipoDefault) shouldBe  false
          equipoActualizado.pozoComun             shouldBe  equipoDefault.pozoComun + 100
        }

        "Si podia hacer todas las misiones, devuelve un equipo diferente, que se ve afectado correctamente" in {
          val equipoActualizado = taberna.entrenar(equipoDefault)
          equipoActualizado.equals(equipoDefault) shouldBe  false
          equipoActualizado.pozoComun             shouldBe  equipoDefault.pozoComun + 100 + 100
        }

      }

    }

  }

}
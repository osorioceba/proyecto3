
package introduccion

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestFiltrarListas extends AnyFunSuite{

  val objEjercicioListas: EjercicioListas = new EjercicioListas()
  test ("Filtrar mayores a 5 en la lista List(1,2,3,4,5,6,7,8,9,10)") {
    assert(objEjercicioListas.filtrarListas("mayor", 5, List(1,2,3,4,5,6,7,8,9,10)) == List(6,7,8,9,10))
  }
  
  test ("Filtrar menores a 5 a lista List(1,2,3,4,5,6,7,8,9,10)") {
    assert(objEjercicioListas.filtrarListas("menor", 5, List(1,2,3,4,5,6,7,8,9,10)) == List(1,2,3,4))
  }

  test ("Filtrar iguales a 3 en la lista List(1,2,3,4,5,6,7,8,9,10)") {
    assert(objEjercicioListas.filtrarListas("igual", 3, List(1,2,3,4,5,6,7,8,9,10)) == List(3))
  }

  test ("Filtrar menoresoiguales a 3 en la lista List(1,2,3,4,5,6,7,8,9,10)")  {
    assert(objEjercicioListas.filtrarListas("menoroigual", 3, List(1,2,3,4,5,6,7,8,9,10)) == List(1,2,3))
  }
  
  test ("Filtrar mayoresoiguales a 7 en la lista List(1,2,3,4,5,6,7,8,9,10)") {
    assert(objEjercicioListas.filtrarListas("mayoroigual", 7, List(1,2,3,4,5,6,7,8,9,10)) == List(7,8,9,10))
  }

  test ("Filtrar diferentes a 8 en la lista List(1,2,3,4,5,6,7,8,9,10)") {
    assert(objEjercicioListas.filtrarListas("diferente", 8, List(1,2,3,4,5,6,7,8,9,10)) == List(1,2,3,4,5,6,7,9,10))
  }

  test ("Error cuando criterio esta mal escrito") {
    assertThrows[IllegalArgumentException] {
      objEjercicioListas.filtrarListas("mayorrr", 5, List(1,2,3,4,5,6,7,8,9,10))
    }
  }
}

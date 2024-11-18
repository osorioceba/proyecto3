
package introduccion

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestRepetirListas extends AnyFunSuite{

  val objEjercicioListas: EjercicioListas = new EjercicioListas()
  test("RepetirListas 0 and List(1,2,3)") {
    assert(objEjercicioListas.repetirListas(List(1,2,3),0) == List(List(),List(),List()))
  }
  test("RepetirListas 1 and List(1,2,3)") {
    assert(objEjercicioListas.repetirListas(List(1,2,3),1) == List(List(1),List(2),List(3)))
  }

  test("RepetirListas 2 and List(1,2,3)") {
    assert(objEjercicioListas.repetirListas(List(1,2,3),2) == List(List(1,1),List(2,2),List(3,3)))
  }

  test("RepetirListas 3 and List(1,2,3)") {
    assert(objEjercicioListas.repetirListas(List(1,2,3),3) == List(List(1,1,1),List(2,2,2),List(3,3,3)))
  }

  test("RepetirListas 4 and List(1,2,3)") {
    assert(objEjercicioListas.repetirListas(List(1,2,3),4) == List(List(1,1,1,1),List(2,2,2,2),List(3,3,3,3)))
  }

  test("RepetirListas 5 and List(1,2,3)") {
    assert(objEjercicioListas.repetirListas(List(1,2,3),5) == List(List(1,1,1,1,1),List(2,2,2,2,2),List(3,3,3,3,3)))
  }

  test("RepetirListas 4 and List(5,6,9)") {
    assert(objEjercicioListas.repetirListas(List(5,6,9),4) == List(List(5,5,5,5),List(6,6,6,6),List(9,9,9,9)))
  }

  test("RepetirLista -4 and List(1,2,3)") {
    assertThrows[IllegalArgumentException] {
      objEjercicioListas.repetirListas(List(1,2,3),-4)
    }
  }

  test("RepetirLista -1 and List(1,2,3)") {
    assertThrows[IllegalArgumentException] {
      objEjercicioListas.repetirListas(List(1,2,3),-1)
    }
  }

  

  
}

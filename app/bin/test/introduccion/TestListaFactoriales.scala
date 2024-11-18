
package introduccion

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestListaFactoriales  extends AnyFunSuite {
  val objListaFactoriales: ListaFactoriales = new ListaFactoriales()

  test("ListaFactoriales of 0 is List(1)") {
    assert(objListaFactoriales.objListaFactoriales(0) == List(1))
  }

  test("ListaFactoriales of 1 is List(1, 1)") {
    assert(objListaFactoriales.objListaFactoriales(1) == List(1, 1))
  }

  test("ListaFactoriales of 2 is List(1, 1, 2)") {
    assert(objListaFactoriales.objListaFactoriales(2) == List(1, 1, 2))
  }

  test("ListaFactoriales of 3 is List(1, 1, 2, 6)") {
    assert(objListaFactoriales.objListaFactoriales(3) == List(1, 1, 2, 6))
  }

  test("ListaFactoriales of 8 is List(1, 1, 2, 6, 24, 120, 720, 5040, 40320)") {
    assert(objListaFactoriales.objListaFactoriales(8) == List(1, 1, 2, 6, 24,
      120, 720, 5040, 40320))
  }

  test("ListaFactoriales of 10 is List(1, 1, 2, 6, 24, 120, 720, 5040, 40320, " +
    "362880, 3628800)") {
    assert(objListaFactoriales.objListaFactoriales(10) == List(1, 1, 2, 6, 24,
      120, 720, 5040, 40320, 362880, 3628800))
  }
}

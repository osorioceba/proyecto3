package introduccion

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestFactorial extends AnyFunSuite {
  val objFactorial: Factorial = new Factorial()

  test("Factorial of 0 is 1") {
    assert(objFactorial.calculateFactorial(0) == 1)
  }

  test("Factorial of 1 is 1") {
    assert(objFactorial.calculateFactorial(1) == 1)
  }

  test("Factorial of 2 is 2") {
    assert(objFactorial.calculateFactorial(2) == 2)
  }

  test("Factorial of 3 is 6") {
    assert(objFactorial.calculateFactorial(3) == 6)
  }

  test("Factorial of 20 is 2432902008176640000") {
    assert(objFactorial.calculateFactorial(20) == 2432902008176640000L)
  }
}

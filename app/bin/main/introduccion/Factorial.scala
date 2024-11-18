package introduccion

class Factorial() {

  def calculateFactorial(n: Int): BigInt = {
    var factorial : BigInt = 1
    for (i <- 1 to n) {
      factorial = factorial * i
    }
    return factorial
  }
}

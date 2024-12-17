package taller

class BuscarLista() {
  // el tiempo de riego y la prioridad del tablon
  type Tablon = (Int, Int, Int)

  // Una finca es un vector de tablones
  type Finca = Vector[Tablon]

  // La distancia entre dos tablones se representa por una matriz
  type Distancia = Vector[Vector[Int]]

  // Una programación de riego es un vector que asocia
  // cada tablon i con su turno de riego (0 es el primer turno,
  // n-1 es el último turno)
  type ProgRiego = Vector[Int]

  // El tiempo de inicio de riego es un vector que asocia
  // cada tablon i con el momento del tiempo en que se riega
  type TiempoInicioRiego = Vector[Int]
  
  def buscarElemento(lista: List[Int], elemento: Int): Boolean = {
  lista match {
    case Nil => false
    case x :: xs => if (x == elemento) true else buscarElemento(xs, elemento)
   }
  }

  def costoRiegoTablon(i: Int, f: Finca, pi: ProgRiego): Int = {
    val tiempoInicio = tIR(f, pi)(i)
    val tiempoFinal = tiempoInicio + treg(f, i)
    if (tsup(f, i) - treg(f, i) >= tiempoInicio) {
      tsup(f, i) - tiempoFinal
    } else {
      prior(f, i) * (tiempoFinal - tsup(f, i))
    }
  }

  def costoRiegoFinca(f: Finca, pi: ProgRiego): Int = {
    (0 until f.length).map(i => costoRiegoTablon(i, f, pi)).sum
  }

  def costoMovilidad(f: Finca, pi: ProgRiego, d: Distancia): Int = {
    (0 until pi.length - 1).map(j => d(pi(j))(pi(j + 1))).sum
  }

  def generarProgramacionesRiego(f: Finca): Vector[ProgRiego] = {
    // Dada una finca de n tablones, devuelve todas las
    // posibles programaciones de riego de la finca
    val indices = (0 until f.length).toVector
    indices.permutations.toVector
  }



}

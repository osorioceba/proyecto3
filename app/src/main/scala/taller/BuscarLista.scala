package taller
import scala.util.Random
import scala.collection.parallel.CollectionConverters._

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
// Si t : TiempoInicioRiego y t.length == n, t(i) es la hora a
  // la que inicia a regarse el tablon i
  val random = new Random()

  def fincaAlazar(long: Int): Finca = {
    // Crea una finca de long tablones,
    // con valores aleatorios entre 1 y long * 2 para el tiempo
    // de supervivencia, entre 1 y long para el tiempo
    // de regado y entre 1 y 4 para la prioridad
    val v = Vector.fill(long)(
      (random.nextInt(long * 2) + 1, 
      random.nextInt(long) + 1, 
      random.nextInt(4) + 1)
    )
    
  }
  
def distanciaAlazar(long: Int): Distancia = {
    // Crea una matriz de distancias para una finca
    // de long tablones, con valores aleatorios entre
    // 1 y long * 3
    val v = Vector.fill(long, long)(random.nextInt(long * 3) + 1)
    Vector.tabulate(long, long)((i, j) =>
      if (i < j) v(i)(j)
      else if (i == j) 0
      else v(j)(i)
    )
  }


def tsup(f: Finca, i: Int): Int = {
    f(i)._1
  }

  def treg(f: Finca, i: Int): Int = {
    f(i)._2
  }

  def prior(f: Finca, i: Int): Int = {
    f(i)._3
  }
  def tIR(f: Finca, pi: ProgRiego): TiempoInicioRiego = {
    // Dada una finca f y una programación de riego pi,
    // y f.length == n, tIR(f, pi) devuelve t: TiempoInicioRiego
    // tal que t(i) es el tiempo en que inicia el riego del
    // tablon i de la finca f según pi
    val tiempos = Array.fill(f.length)(0)
    for (j <- 1 until pi.length) {
      val prevTablon = pi(j - 1)
      val currTablon = pi(j)
      tiempos(currTablon) = tiempos(prevTablon) + treg(f, prevTablon)
    }
    tiempos.toVector
  }

  def ProgramacionRiegoOptimo(f: Finca, d: Distancia): (ProgRiego, Int) = {
    // Dada una finca devuelve la programación
    // de riego óptima
    val programaciones = generarProgramacionesRiego(f)
    val costos = programaciones.map(pi =>
      (pi, costoRiegoFinca(f, pi) + costoMovilidad(f, pi, d))
    )
    costos.minBy(_._2)
  }

  def costoRiegoFincaPar(f: Finca, pi: ProgRiego): Int = {
    // Devuelve el costo total de regar una finca f dada una
    // programación de riego pi, calculando en paralelo
    (0 until f.length).par.map(i => costoRiegoTablon(i, f, pi)).sum
  }

  def costoMovilidadPar(f: Finca, pi: ProgRiego, d: Distancia): Int = {
    // Calcula el costo de movilidad de manera paralela
    (0 until pi.length - 1).par.map(j => d(pi(j))(pi(j + 1))).sum
  }

  def generarProgramacionesRiegoPar(f: Finca): Vector[ProgRiego] = {
    // Genera las programaciones posibles de manera paralela
    val indices = (0 until f.length).toVector
    indices.permutations.toVector.par.toVector
  }

  def ProgramacionRiegoOptimoPar(f: Finca, d: Distancia): (ProgRiego, Int) = {
    // Dada una finca, calcula la programación óptima de riego
    val programaciones = generarProgramacionesRiegoPar(f)
    val costos = programaciones.par.map(pi =>
      (pi, costoRiegoFincaPar(f, pi) + costoMovilidadPar(f, pi, d))
    )
    costos.minBy(_._2)
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

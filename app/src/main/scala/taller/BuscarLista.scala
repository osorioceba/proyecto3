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

}

package taller
import scala.util.Random

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
    v
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

}

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
}

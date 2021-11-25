import java.awt.Color

object Constants {
  type BoardState = Vector[Vector[Square]]
  type Row = Vector[Square]
  type Col = Square

  val SIZE  = 10

  val MOVES = 23

  val TOTAL_SQUARES: Double = SIZE * SIZE
  val WINDOW_SIZE = 800
  val colors: List[Color] = List(Color.BLUE, Color.GREEN, Color.RED, Color.MAGENTA)

}

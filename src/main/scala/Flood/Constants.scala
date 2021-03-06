package Flood

import java.awt.Color
import java.util.concurrent.ForkJoinPool

object Constants {
  type BoardState = Vector[Vector[Square]]
  type Row = Vector[Square]
  type Col = Square

  val SIZE = 3

  val MOVES = 10
  val pool: ForkJoinPool = new ForkJoinPool()

  val TOTAL_SQUARES: Double = SIZE * SIZE
  val WINDOW_SIZE = 800
  val colors: List[Color] = List(Color.BLUE, Color.GREEN, Color.RED)
}

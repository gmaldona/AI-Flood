import scala.+:
import java.awt.Color
import java.util.concurrent.ThreadLocalRandom
import Constants.{ BoardState, Row }

object Board {

  val board: BoardState = createBoard

  /** Creates an Randomized Board of size Constants.SIZE and colors within Constants.colors */
  def createBoard: BoardState = {
    var rowVec: BoardState = Vector();
    for (row <- 0 until Constants.SIZE) {
      var colVec: Row = Vector()
      for (col <- 0 until Constants.SIZE) {
        colVec = colVec :+ new Square(row, col, Constants.colors(ThreadLocalRandom.current().nextInt(Constants.colors.length)))
      }
      rowVec = rowVec :+ colVec
    }
    rowVec
  }

  /** Gets the first square (top left corner) */
  def getFirstSquare(boardState: BoardState): Square = if (boardState(0).nonEmpty) boardState.head.head else new Square(0, 0, Color.BLACK)

  /** Displays board to the stdout */
  def displayBoard(board: BoardState): Unit = {
    for (row <- board) {
      for (col <- row) {
        print(col + " ")
      }
      println()
    }
  }

}

package Flood

import java.awt.Color
import java.util.concurrent.ThreadLocalRandom
import Constants.{BoardState, Row}

object Board {

  val board: BoardState = createBoard

  /** Creates an Randomized Board of size Constants.SIZE and colors within Constants.colors */
  def createBoard: BoardState = {
    var rowVec: BoardState = Vector()
    for (row <- 0 until Constants.SIZE) {
      var colVec: Row = Vector()
      for (col <- 0 until Constants.SIZE) {
        colVec = colVec :+ new Square(row, col, Constants.colors(ThreadLocalRandom.current().nextInt(Constants.colors.length)))
      }
      rowVec = rowVec :+ colVec
    }
    rowVec
  }

  def getCopy(board: BoardState, changeRow: Int, changeCol: Int, changeColor: Color): BoardState = {
    var boardCopy: BoardState = Vector()
      for (row <- 0 until Constants.SIZE) {
        var colVec: Row = Vector()
        for (col <- 0 until Constants.SIZE) {
          if (col == changeCol && row == changeRow) {
            colVec = colVec :+ new Square(row, col, changeColor)
          } else {
            colVec = colVec :+ new Square(row, col, board(row)(col).Color)
          }
        }
        boardCopy = boardCopy :+ colVec
      }
      boardCopy
  }

  def getDistinctColors(board: BoardState) : List[Color] = {
    var boardColors: List[Color] = List()
    for (row <- board)
      for (col <- row)
        boardColors = boardColors :+ col.Color
    boardColors.distinct
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

  def displayBoard(board: Array[Array[Square]]) : Unit = {
    for (row <- board) {
      for (col <- row) {
        print(col + " ")
      }
      println()
    }
  }

  def vecToArr(vec: BoardState): Array[Array[Square]] = {
    var array = new Array[Array[Square]](Constants.SIZE)
    for (row <- 0 until Constants.SIZE) {
      var rowArr = new Array[Square](Constants.SIZE)
      for (col <- 0 until Constants.SIZE) {
          rowArr = rowArr :+ vec(row)(col)
      }
      array = array :+ rowArr
    }
    array
  }

  def arrToVec(arr: Array[Array[Square]]): BoardState = {
    var vector: BoardState = Vector()
    for (row <- 0 until arr.length) {
      var rowVec: Vector[Square] = Vector()
      for (col <- 0 until arr(0).length) {
        if (arr(row)(col) != null) rowVec = rowVec :+ arr(row)(col)
      }
      vector = vector :+ rowVec
    }
    vector
  }

}

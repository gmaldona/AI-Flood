import scala.:+
import java.util.ArrayList
import Constants.{BoardState, Col, Row}

import java.awt.Color
import java.util
import java.util.concurrent.ForkJoinTask.invokeAll
import java.util.concurrent.{ForkJoinTask, RecursiveAction}

class Node(parentBoardState: BoardState, selectedColor: Color, height: Int) extends RecursiveAction {

  var childrenNodes: List[Node] = List()
  var score: Double = 0.0
  var chainingSquares: List[Square] = List()
  var Height: Int = height
  var board: BoardState = boardState

  /** Make deep copy (new square objects) of parent board state */
  def boardState: BoardState = {
    var board: BoardState = Vector()
    for (row <- parentBoardState.indices) {
      var rowVec: Row = Vector()
      for (col <- parentBoardState(0).indices) {
        val color: Color = parentBoardState(row)(col).Color
        rowVec = rowVec :+ new Square(row, col, color)
      }
      board = board :+ rowVec
    }
    board
  }

  /** Gets distinct colors on the board state */
  def getDistinctColors: List[Color] = {
    var boardColors: List[Color] = List()
    for (row <- board)
      for (col <- row)
        boardColors = boardColors :+ col.Color
    boardColors.distinct
  }

  /** Changes all of the Squares on the board's visited flag to false */
  def resetVisit(): Unit = {
    for (row <- board.indices) {
      for (col <- board(0).indices) {
        board(row)(col).setVisited(false)
      }
    }
  }

  /** Finds all chaining Squares adjacent to each other */
  def findChainingSquares(row: Int, col: Int): List[Square] = {

    var neighborsList: List[Square] = List()
    val north: Option[Square] = if (row > 0) Option(board(row - 1)(col)) else None
    val west:  Option[Square] = if (col > 0) Option(board(row)(col - 1)) else None
    val south: Option[Square] = if (row < board.length - 1)    Option(board(row + 1)(col)) else None
    val east:  Option[Square] = if (col < board(0).length - 1) Option(board(row)(col + 1)) else None

    val currentSquare = board(row)(col)
    if (! currentSquare.visited) {
      neighborsList = neighborsList :+ currentSquare
      board(currentSquare.Row)(currentSquare.Col).setVisited(true)
    }
    val neighbors = List(north, west, east, south)

    for (n <- neighbors) {
      n match {
        case Some(neighbor) => if (! neighbor.visited && currentSquare.sameColor(neighbor)) {
          neighborsList = neighborsList :+ neighbor
          board(neighbor.Row)(neighbor.Col).setVisited(true)
        }
        case None =>
      }
    }

    if (neighborsList.isEmpty) neighborsList else {
      var tempList: List[Square] = List()
      println(neighborsList)
      for (neighbor <- neighborsList) {
        if (neighbor != currentSquare) {
          val chainingSquares: List[Square] = findChainingSquares(neighbor.Row, neighbor.Col)
          tempList = tempList :++ chainingSquares
        }
      }
      tempList :++ neighborsList
    }
  }

  /** Recursive Action */
  override def compute(): Unit = {
    println(height)
    chainingSquares = findChainingSquares(0, 0)
    for (square <- chainingSquares) {
      val s: Square = board(square.Row)(square.Col)
      s.setColor(selectedColor)
      square.setColor(selectedColor)
    }
    val boardColors = getDistinctColors
    resetVisit
    chainingSquares = findChainingSquares(0, 0)
    score = chainingSquares.length.asInstanceOf[Double] / Constants.TOTAL_SQUARES

    if (height == Constants.MOVES && score != 1.0) {
      tryUnfork()
    }

    else if (height < Constants.MOVES && score != 1.0) {
      for (color <- boardColors) {
        if (selectedColor != color) {
          val childNode: InternalNode = new InternalNode(this, height + 1, color)
          childrenNodes = childrenNodes :+ childNode
          ForkJoinTask.invokeAll(childNode)
        }
      }
    }



  }



}
/**
 *  Game Plan -> Changes to a state happen in the node that contains the state, no modifications in the parent node or any child node
 *               When inheriting states from parent or to child, we make copies !
 */
case class HeadNode(override val boardState: BoardState) extends Node(boardState, Board.getFirstSquare(boardState).Color, 0) {

  override def compute(): Unit = super.compute()

}

case class InternalNode(parentNodeInternal: Node, height: Int, selectedColor: Color) extends Node(parentNodeInternal.board, selectedColor, parentNodeInternal.Height + 1) {
  override def compute(): Unit = super.compute()
}




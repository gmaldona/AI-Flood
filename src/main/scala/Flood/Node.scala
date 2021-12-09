package Flood

import Flood.Constants.{BoardState, Row, pool}

import java.awt.Color
import java.util.concurrent.{ForkJoinTask, RecursiveAction}

@SerialVersionUID(114L)
class Node(parentBoardState: BoardState, selectedColor: Color, height: Int)
  extends RecursiveAction with Serializable  {

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

  def surroundingColors(chainingSquares: List[Square]): List[Color] = {
    var colors: List[Color] = List()
    for (square <- chainingSquares) {

      val northColor: Option[Color] = if (square.Row > 0) Some(board(square.Row - 1)(square.Col).Color) else None
      val westColor : Option[Color] = if (square.Col > 0) Some(board(square.Row)(square.Col - 1).Color) else None
      val southColor: Option[Color] = if (square.Row < board.length - 1)    Some(board(square.Row + 1)(square.Col).Color) else None
      val eastColor : Option[Color] = if (square.Col < board(0).length - 1) Some(board(square.Row)(square.Col + 1).Color) else None

      val currentColor: Color = board(square.Row)(square.Col).Color

      val optionColors = List(northColor, westColor, southColor, eastColor, currentColor)

      for (c <- optionColors) {
        c match {
          case Some(color) => colors = colors :+ color.asInstanceOf[Color]
          case _: Color    => colors = colors :+ c.asInstanceOf[Color]
          case None        =>
        }
      }
    }
    colors.distinct.filter( color => color != selectedColor )
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
    chainingSquares = findChainingSquares(0, 0)
    for (square <- chainingSquares) {
      val s: Square = board(square.Row)(square.Col)
      s.setColor(selectedColor)
      square.setColor(selectedColor)
    }

    val boardColors = getDistinctColors
    val colorsSurrounding: List[Color] = surroundingColors(chainingSquares)

    resetVisit()
    chainingSquares = findChainingSquares(0, 0)
    score = chainingSquares.length.asInstanceOf[Double] / Constants.TOTAL_SQUARES

//    this match {
//      case _: InternalNode => {
//        val parentNode: Node = this.asInstanceOf[InternalNode].parentNodeInternal
//        if (score < parentNode.score) return
//      }
//      case _: HeadNode => {
//        val headNode: HeadNode = this.asInstanceOf[HeadNode]
//        for (color <- colorsSurrounding) {
//          val childNode: InternalNode = InternalNode(Node.this, height + 1, color)
//          headNode.childrenNodes = headNode.childrenNodes :+ childNode
//
//          val thread = new Thread {
//            override def run(): Unit = {
//              childNode.invoke()
//            }
//          }
//
//          headNode.threads = headNode.threads :+ thread
//        }
//        for (thread <- headNode.threads) {
//          thread.run()
//          return
//        }
//      }
//    }

    if (height == Constants.MOVES && score != 1.0) { return }

    else if (height == Constants.MOVES / 2 && score < 0.30) { return }
    if (height < Constants.MOVES && score != 1.0) {
      for (color <- colorsSurrounding) {
        if (selectedColor != color) {
          val childNode: InternalNode = new InternalNode(this, height + 1, color)
          childrenNodes = childrenNodes :+ childNode
          pool.submit(childNode)
        }
      }
      for (child <- childrenNodes) child.join()
    }



  }



}
/**
 *  Game Plan -> Changes to a state happen in the node that contains the state, no modifications in the parent node or any child node
 *               When inheriting states from parent or to child, we make copies !
 */
case class HeadNode(override val boardState: BoardState) extends Node(boardState, Board.getFirstSquare(boardState).Color, 0) {

  var threads: List[Thread] = List()

  override def compute(): Unit = super.compute()

}

case class InternalNode(parentNodeInternal: Node, height: Int, selectedColor: Color) extends Node(parentNodeInternal.board, selectedColor, parentNodeInternal.Height + 1) {
  override def compute(): Unit = super.compute()
}




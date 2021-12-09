package Flood

import Flood.Constants.BoardState

import java.awt.Color
import javax.swing.SwingUtilities

object Main {

  def main(args: Array[String]): Unit = {
    // TODO: Networking, Optimization (deleting bad branches) -> saves memory, for each color -> set a thread up
    val testingBoard: BoardState = Vector(
      Vector(new Square(0, 0, Color.BLUE), new Square(0, 1, Color.GREEN), new Square(0, 2, Color.BLUE)),
      Vector(new Square(1, 0, Color.GREEN), new Square(1, 1, Color.RED), new Square(1, 2, Color.BLUE)),
      Vector(new Square(2, 0, Color.GREEN), new Square(2, 1, Color.BLUE), new Square(2, 2, Color.BLUE))
    )

    run()
  }

  def serverRun(boardState: BoardState): List[Node] = { 
    Board.displayBoard(boardState)
    StateTree.setup(boardState)
    val headNode: Node = StateTree.head
    StateTree.findFirstWinningNode(headNode)
    val winningNode: Option[Node] = StateTree.winningNode
    val winningBranch: List[Node] = if (winningNode.nonEmpty) StateTree.findWinningBranch(winningNode.get) else List()
    if (winningBranch.nonEmpty) winningBranch else List()
  }

  def run(): Unit = {
    val board: BoardState = Board.createBoard
    GUI.startingBoard = board
    Thread.sleep(1000)
    SwingUtilities.invokeLater(() => {
      GUI.run()
    })


    val winningBranch: List[Node] = serverRun(board) 

    if (winningBranch.nonEmpty) {
      println("Solving ... ")
      GUI.setWinningBranch(winningBranch)
      GUI.repaint()
    }

  }

}

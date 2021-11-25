import java.awt.Color

import Constants.BoardState

object Main {

  def main(args: Array[String]): Unit = {

    val testingBoard: BoardState = Vector(
      Vector( new Square(0, 0, Color.BLUE), new Square(0, 1, Color.BLUE), new Square(0, 2, Color.BLUE) ),
      Vector( new Square(1, 0, Color.BLUE), new Square(1, 1, Color.RED), new Square(1, 2, Color.BLUE) ),
      Vector( new Square(2, 0, Color.GREEN), new Square(2, 1, Color.BLUE), new Square(2, 2, Color.BLUE) )
    )

    val board: BoardState = Board.createBoard

    Board.displayBoard(board)
    StateTree.setup(board)
    val headNode: Node = StateTree.head

    StateTree.findFirstWinningNode(headNode)
    val winningNode: Option[Node] = StateTree.winningNode
    val winningBranch: List[Node] = if (winningNode.nonEmpty) StateTree.findWinningBranch(winningNode.get) else List()

    if (winningBranch.nonEmpty) {
      GUI.setWinningBranch(winningBranch)
      GUI.run()
      Thread.sleep(1500)
      GUI.repaint()
    }

    println()
  }

}

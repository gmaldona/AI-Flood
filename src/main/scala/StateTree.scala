import Constants.BoardState

import java.awt.Color
import java.util.concurrent.ForkJoinPool
import scala.:+

object StateTree {

  var head: HeadNode = new HeadNode(Vector(Vector()))
  val pool: ForkJoinPool = new ForkJoinPool()
  var winningFound: Boolean = false
  var winningNode: Option[Node] = None

  /** Sets up the tree and invokes all RecursiveActions */
  def setup(boardState: BoardState): Unit = {
    head = new HeadNode(boardState)
    pool.invoke(head)
  }

  def findShortestMovesNode(node: Node): Unit = {
    if (node.score == 1.0) {
      winningNode match {
        case None    => winningNode = Some(node)
        case Some(n) => if (node.Height < n.Height) winningNode = Some(node)
        return
      }
    }

    if (node.childrenNodes.isEmpty) return
    for (childNode <- node.childrenNodes) {
      findShortestMovesNode(childNode)
    }

  }

  /** Finds the first node with a score of 1.0 and stores the node in the winning node reference */
  def findFirstWinningNode(node: Node): Unit = {

    if (node.score == 1.0) {
      winningNode match {
        case None => winningNode = Some(node)
        case _    =>
          return
      }
    }

    if (node.childrenNodes.isEmpty) return
    winningNode match {
      case None => for (childNode <- node.childrenNodes) {
        findFirstWinningNode(childNode)
      }
      case _ =>
    }
  }

  /** Finds the entire branch of the winning Node */
  def findWinningBranch(winningNode: Node): List[Node] = {
    val winningBranch: List[Node] = List()

    winningNode match {
      case _: InternalNode => findWinningBranch(winningNode.asInstanceOf[InternalNode].parentNodeInternal) :++ (winningBranch :+ winningNode)
      case _: HeadNode     => winningBranch :+ winningNode
    }
  }
}

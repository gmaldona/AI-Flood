package Flood

import Flood.Constants.{BoardState, pool}

import java.util.concurrent.ForkJoinPool

object StateTree {

  var head: HeadNode = new HeadNode(Vector(Vector()))
  var winningFound: Boolean = false
  var winningNode: Option[Node] = None

  /** Sets up the tree and invokes all RecursiveActions */
  def setup(boardState: BoardState): Unit = {
    head = new HeadNode(boardState)
    head.invoke()
    head.join()
  }

  def findShortestMovesNode(node: Node): Unit = {
    if (node.score == 1.0) {
      winningNode match {
        case None => winningNode = Some(node)
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
    println(node.score )
    if (node.score == 1.0) {
      winningNode match {
        case None => winningNode = Some(node)
        case _ =>
          return
      }
    }

    winningNode match {
      case None => for (childNode <- node.childrenNodes) {
        findFirstWinningNode(childNode)
      }
      case _ =>
    }
  }

  /** Finds the entire branch of the winning Flood.Node */
  def findWinningBranch(winningNode: Node): List[Node] = {
    val winningBranch: List[Node] = List()

    winningNode match {
      case _: InternalNode => findWinningBranch(winningNode.asInstanceOf[InternalNode].parentNodeInternal) :++ (winningBranch :+ winningNode)
      case _: HeadNode => winningBranch :+ winningNode
    }
  }
}

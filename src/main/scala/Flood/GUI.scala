package Flood

import Flood.Constants.BoardState

import java.awt.{Color, Graphics, Graphics2D}
import javax.swing.JFrame

object GUI extends JFrame {
  val padding = 160
  var spacing: Double = 0.0
  var winningBranch: List[Node] = List()
  var drawingNode: Option[Node] = None
  var startingBoard: BoardState = Vector()
  var isDone: Boolean = false

  def setWinningBranch(branch: List[Node]): Unit = winningBranch = branch

  def run(): Unit = {
    spacing = (Constants.WINDOW_SIZE - padding).asInstanceOf[Double] / Constants.SIZE.asInstanceOf[Double]
    setLocationRelativeTo(null)
    setSize(Constants.WINDOW_SIZE, Constants.WINDOW_SIZE)
    setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
    setTitle("FLOOD !")
    setVisible(true)
    drawingNode = Some(HeadNode(startingBoard))
  }


  override def paint(g: Graphics): Unit = {

    drawingNode match {
      case None => return
      case _ =>
    }
    val board: BoardState = drawingNode.get.board
    val g2d: Graphics2D = g.asInstanceOf[Graphics2D]
    try {
      for (row <- 0 until Constants.SIZE) {
        for (col <- 0 until Constants.SIZE) {
          g2d.setColor(board(row)(col).Color)
          g2d.fillRect(Math.floor((col * spacing) + (padding / 2)).toInt, Math.floor((row * spacing) + padding / 2).toInt, spacing.toInt, spacing.toInt)
        }
      }
    } catch {
      case e: Exception => e.printStackTrace()
      case e: Throwable => e.printStackTrace()
    }
    g2d.setColor(Color.BLACK)
    g2d.drawString("© Gregory Maldonado CSC 375 Parallel Computing", Constants.WINDOW_SIZE - 400, Constants.WINDOW_SIZE - 25)
    repaint()
  }

  override def repaint(): Unit = {
    if (winningBranch.nonEmpty) {
      drawingNode = Option(winningBranch.head)
      winningBranch = winningBranch.drop(1)
    }
    super.repaint()
    try {
      Thread.sleep(2000)
    }
  }
}

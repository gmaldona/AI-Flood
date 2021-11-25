import java.awt.{BorderLayout, Color, Dimension, Frame, Graphics, Graphics2D, GraphicsDevice, GraphicsEnvironment}
import javax.swing.{JFrame, JPanel}
import Constants.BoardState

object GUI extends JFrame {
  val padding = 150
  var spacing: Double = 0.0
  var winningBranch: List[Node] = List()
  var drawingNode: Option[Node] = None

  def setWinningBranch(branch: List[Node]): Unit = winningBranch = branch

  def run(): Unit = {
    spacing = (Constants.WINDOW_SIZE - padding).asInstanceOf[Double] / Constants.SIZE.asInstanceOf[Double]
    setLocationRelativeTo(null)
    setSize(Constants.WINDOW_SIZE,Constants.WINDOW_SIZE )
    //setExtendedState(Frame.MAXIMIZED_BOTH)
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    setTitle("FLOOD !")
    setVisible(true)
  }

  override def paint(g: Graphics): Unit = {

  drawingNode match {
    case None => return
    case _    =>
  }
    val board: BoardState = drawingNode.get.board
    val g2d: Graphics2D = g.asInstanceOf[Graphics2D]
    try {
      for (row <- 0 until Constants.SIZE) {
        for (col <- 0 until Constants.SIZE) {
          g2d.setColor(board(row)(col).Color)
          g2d.fillRect( Math.ceil((col * spacing) + (padding / 2)).toInt, Math.ceil((row * spacing) + padding / 2).toInt, spacing.toInt, spacing .toInt)
        }
      }
    }

    repaint()
  }

  override def repaint(): Unit = {
    if (winningBranch.nonEmpty) {
      drawingNode = Option(winningBranch.head)
      winningBranch = winningBranch.drop(1)
    }
    super.repaint()
    try {
      Thread.sleep(1000)
    }
  }

}

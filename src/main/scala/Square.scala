import java.awt.Color;

class Square(row: Int, col: Int, defaultColor: Color) {

  var visited: Boolean = false
  var red: Int = defaultColor.getRed
  var green: Int = defaultColor.getGreen
  var blue: Int = defaultColor.getBlue

  def Row: Int = row
  def Col: Int = col
  var Color: Color = defaultColor

  def setVisited(hasVisited: Boolean): Unit = visited = hasVisited

  def setColor(color: Color): Unit = {
    red   = color.getRed
    green = color.getGreen
    blue  = color.getBlue
    Color = color
  }

  override def toString: String = {
    Color match {
      case java.awt.Color.RED   => "red"
      case java.awt.Color.BLUE  => "blue"
      case java.awt.Color.GREEN => "green"
      case _                    => "NIL"
    }
  }

  def sameColor(otherSquare: Square): Boolean = { red == otherSquare.red && green == otherSquare.green && blue == otherSquare.blue }

  def sameColor(color: Color): Boolean = { red == color.getRed && green == color.getGreen && blue == color.getBlue }

}

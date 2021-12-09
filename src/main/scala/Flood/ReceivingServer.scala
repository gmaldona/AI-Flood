package Flood

import Flood.ServerConstants.{STARTING_IP}
import Flood.Constants.{BoardState}
import Flood.Square
import java.io._
import java.net._
import java.io.File
import java.awt.Color

object ReceivingServer {  

  val hostname: String = InetAddress.getLocalHost().getHostName()

  var initialBoardState: BoardState = Vector()
  var winningBranch: List[Node] = List()

  def reconstructBoard(inStream: ObjectInputStream): BoardState = {
    var board: BoardState = Vector()
    var badRequest: Boolean = false
    while (board.length != Constants.SIZE && badRequest == false) {
      var squareRow: Vector[Square] = Vector()
      while (squareRow.length != Constants.SIZE && badRequest == false) {
        try {
          val square: Square = inStream.readObject().asInstanceOf[Square]
          squareRow = squareRow :+ square
        } catch {
          case eof: EOFException => { println("EOF"); badRequest = true;}
          case e: IOException    => { e.printStackTrace(); badRequest = true;}
          case _: Throwable      => {println("Bad Request"); badRequest = true;}
        }
      }
      board = board :+ squareRow
    }
    board
  }

  def listenForBoard: Unit = {
    val serverSocket: ServerSocket = new ServerSocket(4221)
    println("Starting Server...")
    val socketConnection: Socket   = serverSocket.accept()
    val remoteIPConnection: String = socketConnection.getRemoteSocketAddress.asInstanceOf[InetSocketAddress].getAddress.toString
    println("Established Connection to " + remoteIPConnection)
    val outStream: ObjectOutputStream = new ObjectOutputStream(socketConnection.getOutputStream)
    val inStream : ObjectInputStream  = new ObjectInputStream(socketConnection.getInputStream)

    initialBoardState = reconstructBoard(inStream)

    socketConnection.close
    serverSocket.close
  }

  def sendToWinningServer(winningBranch: List[Node]): Unit = {
    var misses = 0 
    while (misses < 10) {
      Thread.sleep(1000)
      try {
        var port: Int = 0
        for (serverHost <- ServerConstants.SERVER_PORTS.keys) {
          ServerConstants.SERVER_PORTS.get(serverHost) match {
            case Some(serverPort) if serverHost == hostname => {
              port = serverPort.toInt
              val clientSocket: Socket = new Socket(ServerConstants.STARTING_IP, port) 
              val outStream: ObjectOutputStream = new ObjectOutputStream(clientSocket.getOutputStream)
              val inStream : ObjectInputStream  = new ObjectInputStream(clientSocket.getInputStream)
              for (node <- winningBranch) {
                for (squareRow <- node.board) {
                  for (square <- squareRow) {
                    outStream.writeObject("Hello")
                    Thread.sleep(300)
                    outStream.flush()
                  }
                }
              }
              clientSocket.close
              outStream.close
              inStream.close
            }
            case _ => 
          }
        } 


      } catch {
        case e: IOException => { println("Waiting... or Dead"); misses = misses + 1}
        case _: Throwable   =>  {println("Dead"); misses = misses + 1 }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    
    listenForBoard
    Board.displayBoard(initialBoardState)
    //winningBranch = Main.serverRun(initialBoardState)

    //println("Winning Branch Size " + winningBranch.length)
    Thread.sleep(5000)
    val socket: Socket = new Socket(ServerConstants.STARTING_IP, 4000)
    if (socket.isConnected) {
      val remoteIPConnection: String = socket.getRemoteSocketAddress.asInstanceOf[InetSocketAddress].getAddress.toString
      println("Established Connection to " + remoteIPConnection)
      val outStream: ObjectOutputStream = new ObjectOutputStream(socket.getOutputStream)
      val inStream : ObjectInputStream  = new ObjectInputStream(socket.getInputStream)
      outStream.writeObject("Hello") 
    } else {
      println("DEAD") 
    } 


    // sendToWinningServer(winningBranch)


  }

}

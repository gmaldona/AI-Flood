package Flood
import Constants.{BoardState}
import Flood.Board._
import Flood.Square
import java.io._
import java.net._

import java.awt.Color

object Altair {
  
  val initialGameState: BoardState  = Board.createBoard
  val distinctColors  : List[Color] = Board.getDistinctColors(initialGameState) 
  var initialServerStates: List[BoardState] = initServerStates

  var altairGameState: BoardState = Vector()
  var winningBranch  : List[Node] = List()

  def initServerStates: List[BoardState] = {
    var serverStates: List[BoardState] = List() 
    for (color <- distinctColors) {
      serverStates = serverStates :+ Board.getCopy(initialGameState, 0, 0, color)
    }
    serverStates
  }

  def deconstructGameState(gameState: BoardState, outStream: ObjectOutputStream): Unit =  {
    for (squareRow <- gameState) {
      for (square <- squareRow) {
        outStream.writeObject(square)
        Thread.sleep(300)
        outStream.flush()
      }
    }
  }

  def reconstructBranch(socket: Socket, inStream: ObjectInputStream): List[Node] = {
    var branch: List[Node] = List()
    var badRequest: Boolean = false
    while (socket.isConnected && !socket.isClosed && badRequest == false) {
      var boardState: BoardState = Vector()
      while (board.length != Constants.SIZE && badRequest == false) {
        var squareRow: Vector[Square] = Vector()
        while(squareRow.length != Constants.SIZE && badRequest == false) {
          try {
              println("Looking for a string") 
              val testString: String = inStream.readObject().asInstanceOf[String]
              println(testString)
              //squareRow = squareRow :+ square
          } catch {
            case e  : IOException  => { println("IO Error"); badRequest = true } 
            case eof: EOFException => { println("EOF"); badRequest = true }
            case   _: Throwable    => badRequest = true
          }
        }
        //boardState = boardState :+ squareRow
      }
      //println("Board Coming from PI :)")
      //Board.displayBoard(boardState)
    }
    branch
  }

  def sendInitGameState: Boolean = {
    for (serverHost <- ServerConstants.SERVER_IPs.keys) {
      try {
        ServerConstants.SERVER_IPs.get(serverHost) match {
          case Some(ip) if initialServerStates.nonEmpty => {
            val clientSocket: Socket = new Socket(ip, 4221)
            val outStream: ObjectOutputStream = new ObjectOutputStream(clientSocket.getOutputStream)
            val inStream : ObjectInputStream  = new ObjectInputStream(clientSocket.getInputStream)
            deconstructGameState(initialGameState, outStream)
            initialServerStates = initialServerStates.drop(1)
            outStream.close()
            inStream.close()
            clientSocket.close()
          }
          case _ =>  
        } 
      } catch {
        case e: IOException => println("Is server \'" + serverHost + "\' on?")
        case _: Throwable   => println("Bad Request")
        false
      }
    }
    altairGameState = initialServerStates(0)
    true
  }

  def listenForRequests: Unit = {
    println("Listening for Requests ...")
    val requestCount = ServerConstants.SERVER_IPs.size
    var openThreads: List[Thread]  = List()
    for (request <- 0 until requestCount) {
      val thread = new Thread {
        override def run(): Unit = {
          for (serverHost <- ServerConstants.SERVER_PORTS.keys) {
            ServerConstants.SERVER_PORTS.get(serverHost) match {
              case Some(port) => {
                try {
                  //TODO: SERVER IS NOT LISTENIN TO PI
                  val serverSocket: ServerSocket = new ServerSocket(port.toInt)
                  val socket      : Socket = serverSocket.accept
                  println("Openning Port " + port)
                  val outStream   : ObjectOutputStream = new ObjectOutputStream(socket.getOutputStream)
                  val inStream    : ObjectInputStream  = new ObjectInputStream(socket.getInputStream)
                  val reconstructedBranch: List[Node] = reconstructBranch(socket, inStream)
                } catch {
                  case e: IOException =>
                }
              }
            }
          }
        }
      }
      thread.start
      openThreads = openThreads :+ thread
    }
  }

  def main(args: Array[String]): Unit = {

    if (! sendInitGameState) {
      println("Bad Request, Try Re-Running Program")
      return
    } 

    if (altairGameState.isEmpty) {
      println("Initial Board is empty. Try Re-Running Program")
      return
    }

    //winningBranch = Main.serverRun(altairGameState)
    //println("Winning Branch Size " + winningBranch.length)

    listenForRequests
    
  }

}

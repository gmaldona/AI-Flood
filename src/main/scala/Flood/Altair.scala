package Flood
import Constants.{BoardState}
import Flood.Board._
import Flood.Square
import java.io._
import java.net._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.{Success, Failure} 
import java.util.concurrent.ConcurrentHashMap

import java.awt.Color

object Altair {
  
  val initialGameState: BoardState  = Board.createBoard
  val distinctColors  : List[Color] = Board.getDistinctColors(initialGameState) 
  var initialServerStates: List[BoardState] = initServerStates

  var altairGameState: BoardState = Vector()
  var winningBranch  : List[Node] = List()

  var concurrentMap: ConcurrentHashMap[String, List[Node]] = new ConcurrentHashMap()

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
    while (! badRequest) {
      var boardState: BoardState = Vector()
      for (i <- 0 until Constants.SIZE) {
        var boardRow: Vector[Square] = Vector()
        for (j <- 0 until Constants.SIZE) {
          try {
            val square: Square = inStream.readObject().asInstanceOf[Square]
            boardRow = boardRow :+ square
          } catch {
            case e  : IOException    => { println("IO Error"); badRequest = true }
            case eof: EOFException   => { println("EOF"); badRequest = true } 
            case t  : Throwable      => { t.printStackTrace(); badRequest = true }
          }
        }
        boardState = boardState :+ boardRow
      }
      if (branch.isEmpty && badRequest == false) {
        val node: Node = HeadNode(boardState)
        branch = branch :+ node
        println(node)
      } else if (branch.nonEmpty && badRequest == false) {
        val node: Node = InternalNode(branch.last, branch.last.Height + 1, boardState(0)(0).Color)
        node.board = boardState
        branch = branch :+ node
        println(node)
      }
    }
    println("RECEIVED BRANCH SIZE " + branch.length)
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
    true
  }

  class RespondingServer(_serverHost: String, _port: String) extends Runnable {
    val serverHost = _serverHost 
    val port = _port.toInt
    override def run(): Unit = {
      try {      
        val serverSocket: ServerSocket = new ServerSocket(port)
        val socket      : Socket = serverSocket.accept
        println("Opening Port: " + port)
        val outStream : ObjectOutputStream = new ObjectOutputStream(socket.getOutputStream)
        val inStream  : ObjectInputStream  = new ObjectInputStream(socket.getInputStream)
        val _branch: List[Node] = reconstructBranch(socket, inStream)

        outStream.close
        inStream.close
        socket.close
        serverSocket.close

        concurrentMap.put(_serverHost, _branch)
      } catch {
          case eof: EOFException => println("eof")
          case e: IOException => println("broke")
        }
    }
  }

  def listenForRequests = {
    println("Listening for Requests ...")
    var serverHosts = ServerConstants.SERVER_PORTS
    for (serverhost <- serverHosts.keys) {
      serverHosts.get(serverhost) match {
        case Some(port) => {
          val respondingServer: RespondingServer = new RespondingServer(serverhost, port)
          val thread: Thread = new Thread(respondingServer)
          thread.start
          //threads = threads :+ thread
        }
        case None =>
      }
    }
    //for (thread <- )
    while (concurrentMap.size() < ServerConstants.SERVER_IPs.size) {Thread.sleep(300)} 
  }

  def composeTree(branches: List[List[Node]]): List[List[Node]] = {
    if (branches.isEmpty) return List()
    val headNode: HeadNode = HeadNode(initialGameState)
    var changedBranches: List[List[Node]] = List()
    for (branch <- branches) {
      var changedBranch: List[Node] = branch.tail
      var node: InternalNode = InternalNode(headNode, headNode.Height + 1, branch.head.board(0)(0).Color)
      changedBranch = List(node) ::: changedBranch
      changedBranches = changedBranches :+ changedBranch
    }
    changedBranches
  }

  def main(args: Array[String]): Unit = {

    if (! sendInitGameState) {
      println("Bad Request, Try Re-Running Program")
      return
    } 

    // if (altairGameState.isEmpty) {
    //   println("Initial Board is empty. Try Re-Running Program")
    //   return
    // }

    //winningBranch = Main.serverRun(altairGameState)
    //println("Found a winning branch with size " + winningBranch.length)

    var branches: List[List[Node]] = List()

    listenForRequests
    val keyIter = concurrentMap.keySet.iterator
    while (keyIter.hasNext) {
      val ip = keyIter.next
      branches = branches :+ concurrentMap.get(ip)
    }

    val winningBranches: List[List[Node]] = composeTree(branches)
    var leastMovesBranch: List[Node] = winningBranches.head
    
    for (branch <- winningBranches) {
      if (branch.length < leastMovesBranch.length) {
        leastMovesBranch = branch
      }
    }

    for (node <- leastMovesBranch) {
      println(node)
    }

    Main.serverGUI(leastMovesBranch)

  }

}

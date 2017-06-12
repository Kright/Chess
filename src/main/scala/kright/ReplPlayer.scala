package kright

import kright.chessmodel.{Board, Move}
import Implicits.BoardWithDescription

/**
  * Created by lgor on 6/8/17.
  */
class ReplPlayer extends Player {

  def readInput(board: Board): Move = {
    val s = scala.io.StdIn.readLine()
    val move = Move.parse(board, s)

    if (move.isEmpty) {
      println(s"can't parse : '$s', please write again")
      return readInput(board)
    }

    val validMove = move.filter(_.isValid(board))
    validMove.getOrElse {
      println(s"move '$s' is invalid ${move.get} :(, please write again")
      readInput(board)
    }
  }

  override def doTurn(b: Board): Move = {
    println()
    println(b.consolePicture)

    val move = readInput(b)

    println()
    println(move(b))

    move
  }
}

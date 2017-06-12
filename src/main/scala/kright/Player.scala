package kright

import kright.chessmodel.{Board, Move}
import Implicits.BoardWithDescription

/**
  * Created by lgor on 6/8/17.
  */
object Player {

  val white = 1

  val black = -white

  def isWhite(player: Int) = player == white

  def isBlack(player: Int) = player == black

  def isValid(player: Int) = isWhite(player) || isBlack(player)
}

trait Player {
  def doTurn(b: Board): Move
}

class PrintingBoardPlayer(val p: Player) extends Player {
  override def doTurn(b: Board): Move = {
    val move = p.doTurn(b)
    println()
    println(move)
    println(move(b).consolePicture)
    move
  }
}

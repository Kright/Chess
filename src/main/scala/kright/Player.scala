package kright

import kright.chessmodel.Board
import kright.chessmodel.move.Move

/**
	* Created by lgor on 6/8/17.
	*/
object Player {

	val white: Int = 1

	val black: Int = -white

	def isWhite(player: Int): Boolean = player == white

	def isBlack(player: Int): Boolean = player == black

	def isValid(player: Int): Boolean = isWhite(player) || isBlack(player)
}

trait Player {
	def doTurn(b: Board): Move
}

case class PrintingBoardPlayer(p: Player) extends Player {
	override def doTurn(b: Board): Move = {
		println()
		val move = p.doTurn(b)
		println(move)
		println(move(b).consolePicture)
		move
	}
}

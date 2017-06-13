package kright.chessmodel.figure

import kright.chessmodel.{Board, Position}

/**
	* Created by lgor on 13.06.2017.
	*/
case class Bishop(player: Int) extends BishopMoves {

	override def isBishop: Boolean = true

	override def hashCode(): Int = if (player == 1) 'b' else -'b'

	override def addPossibleMoves(b: Board, x: Int, y: Int, arr: Figure.MovesAccum): Unit =
		addBishopMoves(b, Position(x, y), arr)

	override def possibleMovesCount(b: Board, x: Int, y: Int): Int =
		bishopMovesCount(b, x, y)
}

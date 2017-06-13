package kright.chessmodel.figure

import kright.chessmodel.{Board, Position}

/**
	* Created by lgor on 13.06.2017.
	*/
case class Queen(player: Int) extends RookMoves with BishopMoves {
	override def isQueen: Boolean = true

	override def hashCode(): Int = if (player == 1) 'q' else -'q'

	override def addPossibleMoves(b: Board, x: Int, y: Int, arr: Figure.MovesAccum): Unit = {
		val pos = Position(x, y)
		addRockMoves(b, pos, arr)
		addBishopMoves(b, pos, arr)
	}

	override def possibleMovesCount(b: Board, x: Int, y: Int): Int =
		rockMovesCount(b, x, y) +
			bishopMovesCount(b, x, y)
}

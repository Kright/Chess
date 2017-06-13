package kright.chessmodel.figure

import kright.chessmodel.{Board, Position}

/**
	* Created by lgor on 13.06.2017.
	*/
case class Knight(player: Int) extends KKmoves {
	override def isKnight: Boolean = true

	override def hashCode(): Int = if (player == 1) 'n' else -'n'

	override def addPossibleMoves(b: Board, x: Int, y: Int, arr: Figure.MovesAccum): Unit = {
		val pos = Position(x, y)

		tryAddMove(x - 2, y - 1, pos, b, arr)
		tryAddMove(x - 2, y + 1, pos, b, arr)
		tryAddMove(x + 2, y - 1, pos, b, arr)
		tryAddMove(x + 2, y + 1, pos, b, arr)

		tryAddMove(x - 1, y - 2, pos, b, arr)
		tryAddMove(x + 1, y - 2, pos, b, arr)
		tryAddMove(x - 1, y + 2, pos, b, arr)
		tryAddMove(x + 1, y + 2, pos, b, arr)
	}

	override def possibleMovesCount(b: Board, x: Int, y: Int): Int =
		canAddMove(x - 2, y - 1, b) +
			canAddMove(x - 2, y + 1, b) +
			canAddMove(x + 2, y - 1, b) +
			canAddMove(x + 2, y + 1, b) +
			canAddMove(x - 1, y - 2, b) +
			canAddMove(x + 1, y - 2, b) +
			canAddMove(x - 1, y + 2, b) +
			canAddMove(x + 1, y + 2, b)
}

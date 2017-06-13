package kright.chessmodel.figure

import kright.chessmodel.{Board, Position}

/**
	* Created by lgor on 13.06.2017.
	*/
case class Rook(player: Int) extends RookMoves {

	override def isRook: Boolean = true

	override def hashCode(): Int = if (player == 1) 'r' else -'r'

	override def addPossibleMoves(b: Board, x: Int, y: Int, arr: Figure.MovesAccum): Unit =
		addRockMoves(b, Position(x, y), arr)

	override def possibleMovesCount(b: Board, x: Int, y: Int): Int =
		rockMovesCount(b, x, y)
}

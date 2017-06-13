package kright.chessmodel.move

import kright.chessmodel.figure.{Empty, Figure, Pawn}
import kright.chessmodel.{Board, Position}

/**
	* Created by lgor on 13.06.2017.
	*/
case class Pawn2Fig(from: Position, to: Position, figure: Figure) extends Move {
	assert(!figure.isEmpty)
	assert(!figure.isPawn)

	override def apply(b: Board): Board = {
		b.doTurn(Empty -> from, figure -> to)
	}

	override def makeDescription(b: Board): String = {
		val f1 = b(from)
		val f2 = b(to)
		val symbol = if (f2 == Empty) "-" else "x"
		s"${f1.shortName}$from$symbol$to${figure.shortName}"
	}

	override def isValid(b: Board): Boolean = {
		val f1 = b(from)

		if (!(f1.isPlayerFig(b.currentPlayer) && f1.isPawn)) return false

		f1.possibleMoves(b, from.x, from.y).contains(this)
	}
}

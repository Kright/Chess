package kright.chessmodel.move

import kright.chessmodel.figure._
import kright.chessmodel.{Board, Position}


case class SimpleMove(from: Position, to: Position) extends Move {
	def makeDescription(b: Board): String = {
		val f1 = b(from)
		val f2 = b(to)
		val symbol = if (f2 == Empty) "-" else "x"
		s"${f1.shortName}$from$symbol$to"
	}

	override def apply(b: Board): Board = {
		val figure = b(from)
		b.doTurn(Empty -> from, figure -> to)
	}

	override def isValid(b: Board): Boolean = {
		val f1 = b(from)
		if (!f1.isPlayerFig(b.currentPlayer)) return false
		val f2 = b(to)
		if (f2.isPlayerFig(b.currentPlayer)) return false

		f1.possibleMoves(b, from.x, from.y).contains(this)
	}
}

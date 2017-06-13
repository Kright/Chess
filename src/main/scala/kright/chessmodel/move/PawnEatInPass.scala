package kright.chessmodel.move

import kright.chessmodel.figure.{Empty, Pawn}
import kright.chessmodel.{Board, Position}

/**
	* Created by lgor on 13.06.2017.
	*/
case class PawnEatInPass(from: Position, to: Position, eated: Position) extends Move {
	override def makeDescription(b: Board): String = {
		s"${from}x$to"
	}

	override def isValid(b: Board): Boolean = {
		val fig = b(from)
		if (!fig.isPawn) return false
		val pawn = fig.asInstanceOf[Pawn]

		b(eated).isPawn &&
			b(eated).isPlayerFig(-pawn.player) &&
			b(to).isEmpty &&
			b.previous.exists { case (prevBoard, acts) =>
				prevBoard(eated).isEmpty
			}
	}

	override def apply(board: Board): Board = board.doTurn(Empty -> eated, Empty -> from, board(from) -> to)
}

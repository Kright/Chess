package kright.chessmodel.move

import kright.chessmodel.figure.{Empty, King, Rook}
import kright.chessmodel.{Board, Position}

/**
	* Created by lgor on 13.06.2017.
	*/
case class Castling(toA: Boolean) extends Move {
	override def makeDescription(b: Board): String =
		if (toA)
			"0-0-0"
		else
			"0-0"

	@inline
	private def playerY(player: Int) =
		player match {
			case 1 => 0
			case -1 => 7
		}

	private def positionsAreSafe(positions: Set[Position], board: Board): Boolean =
		board.allPossibleMoves().forall {
			case SimpleMove(from, to) => !positions.contains(to)
			case Pawn2Fig(from, to, figure) => !positions.contains(to)
			case m: PawnEatInPass => true
			case m: Castling => true
		}

	override def isValid(board: Board): Boolean = {
		val info = board.info(board.currentPlayer)
		val y = playerY(board.currentPlayer)

		if (toA) {
			if (!info.castlingAPossible)
				return false

			val fieldsBetweenAreEmpty = (1 to 3).forall(board(_, y).isEmpty)
			if (!fieldsBetweenAreEmpty)
				return false

			val kingFields = (2 to 4).map(Position(_, y)).toSet
			if (!positionsAreSafe(kingFields, this (board)))
				return false

			true
		} else {
			if (!info.castlingAPossible)
				return false

			val fieldsBetweenAreEmpty = (5 to 6).forall(board(_, y).isEmpty)
			if (!fieldsBetweenAreEmpty)
				return false

			val kingFields = (4 to 6).map(Position(_, y)).toSet
			if (!positionsAreSafe(kingFields, this (board)))
				return false

			true
		}
	}

	override def apply(b: Board): Board = {
		val y = if (b.currentPlayer == 1) 0 else 7

		if (toA)
			b.doTurn(
				Empty -> Position(4, y),
				Empty -> Position(0, y),
				King(b.currentPlayer) -> Position(2, y),
				Rook(b.currentPlayer) -> Position(3, y)
			)
		else
			b.doTurn(
				Empty -> Position(4, y),
				Empty -> Position(7, y),
				King(b.currentPlayer) -> Position(6, y),
				Rook(b.currentPlayer) -> Position(5, y)
			)
	}
}

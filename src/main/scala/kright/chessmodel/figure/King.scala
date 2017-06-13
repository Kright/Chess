package kright.chessmodel.figure

import kright.chessmodel.{Board, Position}
import kright.chessmodel.figure.Figure.MovesAccum
import kright.chessmodel.move.Castling

/**
	* Created by lgor on 13.06.2017.
	*/
case class King(player: Int) extends KKmoves {

	override def isKing: Boolean = true

	override def hashCode(): Int = if (player == 1) 'k' else -'k'

	override def addPossibleMoves(board: Board, x: Int, y: Int, arr: MovesAccum): Unit = {
		val pos = Position(x, y)

		if (board.info(player).castlingAPossible) {
			val castling = Castling(toA = true)
			if (castling.isValid(board))
				arr += castling
		}

		if (board.info(player).castlingHPossible) {
			val castling = Castling(toA = false)
			if (castling.isValid(board))
				arr += castling
		}

		tryAddMove(x - 1, y - 1, pos, board, arr)
		tryAddMove(x - 1, y, pos, board, arr)
		tryAddMove(x - 1, y + 1, pos, board, arr)

		tryAddMove(x, y - 1, pos, board, arr)
		tryAddMove(x, y + 1, pos, board, arr)

		tryAddMove(x + 1, y - 1, pos, board, arr)
		tryAddMove(x + 1, y, pos, board, arr)
		tryAddMove(x + 1, y + 1, pos, board, arr)
	}

	override def possibleMovesCount(board: Board, x: Int, y: Int): Int = {
		var counter = 0

		if (board.info(player).castlingAPossible) {
			val castling = Castling(toA = true)
			if (castling.isValid(board))
				counter += 1
		}

		if (board.info(player).castlingHPossible) {
			val castling = Castling(toA = false)
			if (castling.isValid(board))
				counter += 1
		}

		counter +
			canAddMove(x - 1, y - 1, board) +
			canAddMove(x - 1, y, board) +
			canAddMove(x - 1, y + 1, board) +
			canAddMove(x, y - 1, board) +
			canAddMove(x, y + 1, board) +
			canAddMove(x + 1, y - 1, board) +
			canAddMove(x + 1, y, board) +
			canAddMove(x + 1, y + 1, board)
	}
}

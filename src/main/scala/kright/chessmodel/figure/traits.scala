package kright.chessmodel.figure

import kright.chessmodel.{Board, Position}
import kright.chessmodel.move.SimpleMove

/**
	* Created by lgor on 13.06.2017.
	*/
trait KKmoves extends PlayerFig {

	def tryAddMove(x: Int, y: Int, from: Position, board: Board, arr: Figure.MovesAccum): Unit = {
		if (canMoveTo(board, x, y))
			arr += SimpleMove(from, new Position(x, y))
	}

	def canAddMove(x: Int, y: Int, board: Board): Int = if (canMoveTo(board, x, y)) 1 else 0
}

trait LinearFig extends PlayerFig {
	protected def addLinearMoves(dx: Int, dy: Int, board: Board, pos: Position, arr: Figure.MovesAccum): Unit = {
		for (i <- 1 to 7) {
			val x = pos.x + dx * i
			val y = pos.y + dy * i

			if (!canMoveTo(board, x, y)) return

			arr += SimpleMove(pos, new Position(x, y))
			if (board(x, y) != Empty)
				return
		}
	}

	protected def linearMovesCount(dx: Int, dy: Int, board: Board, posX: Int, posY: Int): Int = {
		var count = 0

		for (i <- 1 to 7) {
			val x = posX + dx * i
			val y = posY + dy * i

			if (!canMoveTo(board, x, y)) return count
			count += 1

			if (!board(x, y).isEmpty)
				return count
		}

		count
	}
}

trait RookMoves extends LinearFig {
	def addRockMoves(b: Board, position: Position, arr: Figure.MovesAccum): Unit = {
		addLinearMoves(1, 0, b, position, arr)
		addLinearMoves(-1, 0, b, position, arr)
		addLinearMoves(0, -1, b, position, arr)
		addLinearMoves(0, 1, b, position, arr)
	}

	def rockMovesCount(b: Board, x: Int, y: Int): Int = {
		linearMovesCount(1, 0, b, x, y) +
			linearMovesCount(-1, 0, b, x, y) +
			linearMovesCount(0, -1, b, x, y) +
			linearMovesCount(0, 1, b, x, y)
	}
}

trait BishopMoves extends LinearFig {
	def addBishopMoves(b: Board, position: Position, arr: Figure.MovesAccum): Unit = {
		addLinearMoves(-1, -1, b, position, arr)
		addLinearMoves(-1, 1, b, position, arr)
		addLinearMoves(1, -1, b, position, arr)
		addLinearMoves(1, 1, b, position, arr)
	}

	def bishopMovesCount(b: Board, x: Int, y: Int): Int = {
		linearMovesCount(-1, -1, b, x, y) +
			linearMovesCount(-1, 1, b, x, y) +
			linearMovesCount(1, -1, b, x, y) +
			linearMovesCount(1, 1, b, x, y)
	}
}

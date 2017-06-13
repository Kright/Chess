package kright.ai.winfactor

import kright.Player
import kright.ai.WinFactor
import kright.chessmodel.figure._
import kright.chessmodel.{Board, Position}

/**
	* Created by lgor on 6/10/17.
	*/
class PositionFactor extends WinFactor {
	def isPawn(figure: Figure, player: Int): Boolean =
		figure match {
			case Pawn(p) => p == player
			case _ => false
		}

	def pawnPassedFreeMax(board: Board, x: Int, y: Int, player: Int): Boolean = {
		val enemyInfo = board.info(player)

		enemyInfo.pawnsOnLines(x) == 0 &&
			(x + 1 > 7 || enemyInfo.pawnsOnLines(x + 1) == 0) &&
			(x - 1 < 0 || enemyInfo.pawnsOnLines(x - 1) == 0)
	}

	def withoutEnemyPawns(board: Board, x: Int, player: Int): Boolean =
		(0 until 8).forall(y => !isPawn(board(x, y), -player))

	override def apply(board: Board): Double = {
		// https://habrahabr.ru/post/305604/
		var sum = 0.0

		var whiteQueenPos: Position = null
		var whiteKingPos: Position = null

		var blackQueenPos: Position = null
		var blackKingPos: Position = null

		for (x <- 0 until 8;
		     y <- 0 until 8) {
			val figure = board(x, y)

			figure match {
				case Empty =>

				case Pawn(p) =>
					val f2 = board.safe(x, y + p)
					if (f2.isPlayerFig(p) && f2.isPawn)
						sum += -10 * p
					if (Position.isCenter(x, y))
						sum += 10 * p
					if (pawnPassedFreeMax(board, x, y, p))
						sum += 100 * p

				case Knight(p) =>
					if (Position.isCenter(x, y))
						sum += 27 * p

					sum += 10 * p * figure.possibleMovesCount(board, x, y)

				case Bishop(p) =>
					if (Position.isCenter(x, y))
						sum += 10 * p

					sum += 60 * p * figure.possibleMovesCount(board, x, y)

				case Rook(p) =>
					if (withoutEnemyPawns(board, x, -p))
						sum += 17 * p

					sum += 40 * p * figure.possibleMovesCount(board, x, y)

				case Queen(p) =>
					if (Player.isWhite(p))
						whiteQueenPos = new Position(x, y)
					else
						blackQueenPos = new Position(x, y)

				case King(p) =>
					if (Player.isWhite(p))
						whiteKingPos = new Position(x, y)
					else
						blackKingPos = new Position(x, y)

				case _ => ???
			}
		}

		assert(blackKingPos != null)
		assert(whiteKingPos != null)

		if (whiteQueenPos != null && whiteQueenPos.distanceDiag(blackKingPos) <= 2)
			sum += 100

		if (blackQueenPos != null && blackQueenPos.distanceDiag(whiteKingPos) <= 2)
			sum -= 100

		sum
	}
}



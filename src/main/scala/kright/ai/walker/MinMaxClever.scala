package kright.ai.walker

import kright.ai.{Walker, WinFactor}
import kright.chessmodel._
import kright.chessmodel.figure.Figure
import kright.chessmodel.move._

/**
	* Created by lgor on 12.06.2017.
	*/
class MinMaxClever(val maxRecursionDepth: Int, val interestingCost: Double = 0.5) extends Walker {

	assert(maxRecursionDepth > 0)

	private var variantsCount: Long = 0

	private val boringCost = 1.0

	private def findMove(board: Board, depth: Double, threshold: Double, metrics: WinFactor): (Move, Double) = {
		if (depth <= 0) {
			variantsCount += 1
			return (null, metrics(board))
		}
		board.gameStatus match {
			case GameFinished(winner) => (null, (winner + 1.0) / 2.0)
			case GamePlaying =>
				val player = board.currentPlayer
				val moves = board.allPossibleMoves()
				assert(moves.nonEmpty)

				val interesting = new Figure.MovesAccum()
				val others = new Figure.MovesAccum()

				moves.foreach {
					case m: SimpleMove =>
						if (board(m.to).isEmpty)
							others += m
						else
							interesting += m
					case m: Pawn2Fig =>
						interesting += m
					case m: PawnEatInPass =>
						interesting += m
					case m: Castling =>
						interesting += m
				}

				var bestChance = worstChance(player)
				var bestMove: Move = null

				for (move <- interesting) {
					val (_, chance) = findMove(move(board), depth - interestingCost, bestChance, metrics)

					if (isBetter(chance, bestChance, player)) {
						bestChance = chance
						bestMove = move

						if (isBetter(bestChance, threshold, player)) {
							return (bestMove, bestChance)
						}
					}
				}

				for (move <- others) {
					val (_, chance) = findMove(move(board), depth - boringCost, bestChance, metrics)

					if (isBetter(chance, bestChance, player)) {
						bestChance = chance
						bestMove = move

						if (isBetter(bestChance, threshold, player)) {
							return (bestMove, bestChance)
						}
					}
				}
				(bestMove, bestChance)
		}
	}

	private def worstChance(player: Int): Double =
		player match {
			case 1 => 0 - 0.001
			case -1 => 1 + 0.001
		}

	private def isBetter(chance: Double, than: Double, player: Int): Boolean =
		player match {
			case 1 => chance > than
			case -1 => chance < than
		}

	private def inverse(prob: Double): Double = 1 - prob

	override def doTurn(board: Board, metrics: WinFactor): Move = {
		variantsCount = 0
		val t = System.currentTimeMillis()
		val (move, winChance) = findMove(board, maxRecursionDepth, worstChance(-board.currentPlayer), metrics)

		val dt = System.currentTimeMillis() - t
		println(s"CLEVER\tvariantsCount = $variantsCount, probWinWhite = $winChance, time ${System.currentTimeMillis() - t}ms, perf = ${variantsCount * 1000 / dt} pos/sec")

		move
	}
}

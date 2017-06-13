package kright.ai.walker

import kright.ai.{Walker, WinFactor}
import kright.chessmodel.move.Move
import kright.chessmodel.{Board, GameFinished, GamePlaying}

/**
	* Created by lgor on 12.06.2017.
	*/
class MinMaxWalker(val maxRecursionDepth: Int) extends Walker {

	assert(maxRecursionDepth > 0)

	private var variantsCount: Long = 0

	private def findMove(board: Board, depth: Int, threshold: Double, metrics: WinFactor): (Move, Double) = {
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

				var bestChance = worstChance(player)
				var bestMove: Move = null

				for (move <- moves) {
					val (_, chance) = findMove(move(board), depth - 1, bestChance, metrics)

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
		println(s"variantsCount = $variantsCount, probWinWhite = $winChance, time ${System.currentTimeMillis() - t}ms, perf = ${variantsCount * 1000 / dt} pos/sec")

		move
	}
}

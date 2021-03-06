package kright.ai.walker

import kright.Player
import kright.ai.{Walker, WinFactor}
import kright.chessmodel.Board
import kright.chessmodel.move.Move


class FixedDepthWalker(val recursionDepth: Int) extends Walker {

	private var variantsCount: Long = 0

	private def prob(b: Board, metrics: WinFactor, curDepth: Int): Double = {
		if (curDepth <= 0) {
			variantsCount += 1
			metrics(b)
		} else {
			val moves = b.allPossibleMoves()
			if (moves.isEmpty) {
				//println("moves are empty :(\n" + b.consolePicture)
				return if (Player.isWhite(b.currentPlayer)) 0 else 1
			}
			val (move, winWhite) = moves.map(move => (move, prob(move(b), metrics, curDepth - 1))).maxBy(_._2 * b.currentPlayer)
			winWhite
		}
	}

	override def doTurn(b: Board, metrics: WinFactor): Move = {
		val t = System.currentTimeMillis()
		variantsCount = 0
		val moves = b.allPossibleMoves()
		val (move, winWhite) = moves.map(move => (move, prob(move(b), metrics, recursionDepth - 1))).maxBy(_._2 * b.currentPlayer)

		val dt = System.currentTimeMillis() - t
		println(s"variantsCount = $variantsCount, probWinWhite = $winWhite, time ${System.currentTimeMillis() - t}ms, perf = ${variantsCount * 1000 / dt} pos/sec")

		move
	}
}


class FixedDepthWalkerMultithread(val depth: Int) extends Walker {

	@volatile
	private var variantsCount = 0

	private def prob(b: Board, metrics: WinFactor, curDepth: Int): Double = {
		if (curDepth >= depth) {
			variantsCount += 1
			metrics(b)
		} else {
			val moves = b.allPossibleMoves()
			if (moves.isEmpty) {
				//println("moves are empty :(\n" + b.consolePicture)
				return if (Player.isWhite(b.currentPlayer)) 0 else 1
			}
			val (move, winWhite) = moves.map(move => (move, prob(move(b), metrics, curDepth + 1))).maxBy(_._2 * b.currentPlayer)
			winWhite
		}
	}

	override def doTurn(b: Board, metrics: WinFactor): Move = {
		val t = System.currentTimeMillis()
		val moves = b.allPossibleMoves()
		val (move, winWhite) = moves.toParArray.map(move => (move, prob(move(b), metrics, 1))).maxBy(_._2 * b.currentPlayer)

		println(s"variantsCount = $variantsCount, probWinWhite = $winWhite, time ${System.currentTimeMillis() - t}ms")
		variantsCount = 0

		move
	}
}

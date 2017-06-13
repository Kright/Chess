package kright.ai.winfactor

import kright.ai.WinFactor
import kright.chessmodel.{Board, GameFinished, GamePlaying}

/**
	* Created by lgor on 6/11/17.
	*/
class TotalFactor(val arr: Array[(Double, WinFactor)]) extends WinFactor {

	def this(seq: (Double, WinFactor)*) = this(seq.toArray)

	override def apply(b: Board): Double = {

		b.gameStatus match {
			case GamePlaying =>
				val balance: Double = arr.map { case (weight, factor) => weight * factor(b) }.sum
				// https://habrahabr.ru/post/254753/
				val probWhiteWins = 1.0 / (1.0 + math.exp(-0.7 * balance))
				probWhiteWins

			case GameFinished(1) => 1
			case GameFinished(-1) => 0
			case GameFinished(0) => 0.5
		}
	}
}

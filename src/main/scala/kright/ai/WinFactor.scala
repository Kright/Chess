package kright.ai

import kright.ai.winfactor.{FiguresWeightFactor, PositionFactor, TotalFactor}
import kright.chessmodel._

/**
	* Created by lgor on 6/8/17.
	*/
trait WinFactor {

	def apply(board: Board): Double

	def apply(board: Board, player: Int): Double =
		player match {
			case 1 => apply(board)
			case -1 => 1 - apply(board)
		}
}

object WinFactor {
	def simplest: WinFactor = new TotalFactor(1.0 -> new FiguresWeightFactor())

	def withPosition: WinFactor = new TotalFactor(1.0 -> new FiguresWeightFactor(), 0.01 -> new PositionFactor())
}

package kright.ai.winfactor

import kright.ai.WinFactor
import kright.chessmodel.{Board, PlayerSensitiveInfo}

/**
	* Created by lgor on 6/11/17.
	*/
class FiguresWeightFactor(val pawn: Double = 1,
                          val knight: Double = 4,
                          val bishop: Double = 4,
                          val rook: Double = 6,
                          val queen: Double = 12) extends WinFactor {

	def summ(p: PlayerSensitiveInfo): Double =
		p.pawns * pawn +
			p.rooks * rook +
			p.knights * knight +
			p.bishops * bishop +
			p.queens * queen


	def apply(board: Board): Double = {
		val info = board.info
		summ(info.white) - summ(info.black)
	}
}


package kright.ai

import kright.Player
import kright.chessmodel.Board
import kright.chessmodel.move.Move

/**
	* Created by lgor on 6/8/17.
	*/
case class AI(walker: Walker, metrics: WinFactor) extends Player {

	override def doTurn(board: Board): Move = {
		walker.doTurn(board, metrics)
	}
}

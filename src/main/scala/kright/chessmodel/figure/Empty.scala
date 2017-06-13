package kright.chessmodel.figure

import kright.chessmodel.Board
import kright.chessmodel.figure.Figure.MovesAccum

/**
	* Created by lgor on 13.06.2017.
	*/
object Empty extends Figure {
	override def isPlayerFig(player: Int): Boolean = false

	override def isEmpty: Boolean = true

	override def possibleMovesCount(board: Board, x: Int, y: Int): Int = 0

	override def addPossibleMoves(board: Board, x: Int, y: Int, acc: MovesAccum): Unit = {
		//Nothing
	}

	override def hashCode(): Int = 0
}

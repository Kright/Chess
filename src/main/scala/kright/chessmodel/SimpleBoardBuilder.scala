package kright.chessmodel

import kright.chessmodel.figure._

/**
	* Created by lgor on 13.06.2017.
	*/
class SimpleBoardBuilder() extends BoardBuilder {
	val array: Array[Figure] = (0 until 64).map(i => Empty).toArray

	override def board: Board = new SimpleBoard(1, GamePlaying, None, array.clone())

	override def add(boardActions: Seq[(Figure, Position)]): BoardBuilder = {
		boardActions.foreach {
			case (fig, pos) =>
				array(pos.x + pos.y * 8) = fig
		}
		this
	}
}

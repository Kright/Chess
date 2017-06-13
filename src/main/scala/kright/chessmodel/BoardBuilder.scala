package kright.chessmodel

import Board.BoardAction
import kright.SimpleBoardBuilder

/**
	* Created by lgor on 6/10/17.
	*/
object BoardBuilder {
	implicit def builder2board(b: BoardBuilder): Board = b.board

	def empty: BoardBuilder = new SimpleBoardBuilder()

	def standard: BoardBuilder = empty.add(Board.allFigures)
}

trait BoardBuilder {
	def board: Board

	def add(boardAction: Seq[BoardAction]): BoardBuilder

	def add(action: BoardAction): BoardBuilder = add(Seq(action))
}
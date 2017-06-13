package kright.chessmodel.figure

import kright._
import kright.chessmodel._
import kright.chessmodel.move.{Move, SimpleMove}

import scala.collection.mutable.ArrayBuffer

/**
	* Created by lgor on 6/6/17.
	*/
object Figure {
	type MovesAccum = ArrayBuffer[Move]
}

trait Figure {

	def isPlayerFig(player: Int): Boolean


	def possibleMovesCount(board: Board, x: Int, y: Int): Int

	def addPossibleMoves(board: Board, x: Int, y: Int, acc: Figure.MovesAccum): Unit


	def isEmpty: Boolean = false

	def isKing: Boolean = false

	def isQueen: Boolean = false

	def isPawn: Boolean = false

	def isRook: Boolean = false

	def isKnight: Boolean = false

	def isBishop: Boolean = false


	def shortName: String =
		this match {
			case Empty => "_"
			case k: King => "K"
			case b: Bishop => "B"
			case r: Rook => "R"
			case p: Pawn => "p"
			case q: Queen => "Q"
			case k: Knight => "N"
			case _ => ???
		}

	def playerCaseName: String =
		if (isPlayerFig(Player.white))
			shortName.toUpperCase
		else
			shortName.toLowerCase

	def possibleMoves(board: Board, x: Int, y: Int): Figure.MovesAccum = {
		val moves = new Figure.MovesAccum()
		addPossibleMoves(board, x, y, moves)
		moves
	}
}

trait PlayerFig extends Figure {
	val player: Int

	override def isPlayerFig(p: Int): Boolean = p == player

	protected def canMoveTo(b: Board, x: Int, y: Int): Boolean = Position.isValid(x, y) && !b(x, y).isPlayerFig(player)
}

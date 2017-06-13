package kright.chessmodel.move

import kright.chessmodel.figure._
import kright.chessmodel.{Board, Position}

/**
	* Created by lgor on 6/8/17.
	*/
trait Move {
	def makeDescription(b: Board): String

	def apply(b: Board): Board

	def isValid(b: Board): Boolean
}

object Move {

	def parse(b: Board, s: String): Option[Move] = {
		val simpleMove = "[abcdefgh][12345678][\\-x][abcdefgh][12345678]"

		if (s.matches(simpleMove)) {
			for (from <- Position.parse(s.substring(0, 0 + 2));
			     to <- Position.parse(s.substring(3, 3 + 2))) {
				return Option(SimpleMove(from, to))
			}
			return None
		}

		if (s.matches("p" + simpleMove)) {
			for (from <- Position.parse(s.substring(1, 1 + 2));
			     to <- Position.parse(s.substring(4, 4 + 2))) {
				val eated = new Position(to.x, from.y)

				return Option(PawnEatInPass(from, to, eated))
			}
			return None
		}

		if (s.matches(simpleMove + "[qbrn]")) {
			for (from <- Position.parse(s.substring(0, 2));
			     to <- Position.parse(s.substring(3, 3 + 2))) {
				val newFigure: (Int) => PlayerFig = s.substring(5, 6) match {
					case "q" => Queen.apply
					case "b" => Bishop.apply
					case "r" => Rook.apply
					case "n" => Knight.apply
				}

				return Option(Pawn2Fig(from, to, newFigure(b.currentPlayer)))
			}
			return None
		}

		if (s == "0-0")
			return Option(Castling(toA = false))

		if (s == "0-0-0")
			return Option(Castling(toA = true))

		None
	}
}
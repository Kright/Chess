package kright

import kright.chessmodel.move.{Castling, Move, PawnEatInPass, SimpleMove}
import kright.chessmodel.Position

/**
	* Created by lgor on 6/8/17.
	*/
class Move$Test extends org.scalatest.FunSuite {

	test("testParse") {
		assert(Move.parse(null, "a1-a2") == Option(SimpleMove(new Position(0, 0), new Position(0, 1))))
		assert(Move.parse(null, "pe5-d6") == Option(PawnEatInPass(new Position(4, 4), new Position(3, 5), new Position(3, 4))))
		assert(Move.parse(null, "0-0-0") == Option(Castling(toA = true)))
		assert(Move.parse(null, "0-0") == Option(Castling(toA = false)))
	}

}

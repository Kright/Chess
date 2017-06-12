package kright

import kright.chessmodel.{Move, Position, SimpleMove}

/**
  * Created by lgor on 6/8/17.
  */
class Move$Test extends org.scalatest.FunSuite {

  test("testParse should Parse") {
    assert(Move.parse(null, "a1-a2") == Option(new SimpleMove(new Position(0, 0), new Position(0, 1))))
  }

}

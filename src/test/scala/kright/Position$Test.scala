package kright

import kright.chessmodel.Position
import org.scalatest.FunSuite

/**
  * Created by lgor on 6/8/17.
  */
class Position$Test extends FunSuite {

  test("equals") {
    assert(new Position(1, 2) == new Position(1, 2))
    assert(new Position(1, 2) != new Position(2, 2))
    assert(new Position(2, 1) != new Position(2, 2))
    assert(new Position(1, 1) != new Object())
  }

  test("testParse") {
    assert(Position.parse("a1") == Option(new Position(0, 0)))
    assert(Position.parse("a8") == Option(new Position(0, 7)))
    assert(Position.parse("h8") == Option(new Position(7, 7)))
  }

  test("testIsValid") {
    assert(Position.isValid(0))
    assert(Position.isValid(7))
    assert(!Position.isValid(-1))
    assert(!Position.isValid(8))
  }

  test("testIsValidMulty") {
    assert(Position.isValid(7, 7))
    assert(Position.isValid(0, 0))
    assert(!Position.isValid(0, 8))
    assert(!Position.isValid(8, 0))
  }
}

package kright

import kright.chessmodel.Board.BoardAction
import kright.chessmodel._

/**
  * todo draw game state
  *
  * Created by lgor on 6/6/17.
  */
class SimpleBoard(val currentPlayer: Int,
                  val gameStatus: GameStatus,
                  val previous: Option[(Board, Seq[BoardAction])],
                  private val arr: Array[Figure]) extends Board {

  assert(Player.isValid(currentPlayer))
  assert(arr.size == 64)

  override def apply(x: Int, y: Int): Figure = arr(x + y * 8)

  override def doTurn(actions: BoardAction*): Board = {
    assert(gameStatus.playing)
    val array = arr.clone()

    var kingsCountDelta = 0

    for (act <- actions) {
      val pos = act._2.x + act._2.y * 8
      if (arr(pos).isKing) kingsCountDelta -= 1
      array(pos) = act._1
      if (act._1.isKing) kingsCountDelta += 1
    }

    val gs = if (kingsCountDelta == 0) GamePlaying else GameFinished(currentPlayer)

    new SimpleBoard(-currentPlayer, gs, Option(this, actions), array)
  }

  override val hashCode: Int = {
    val long = (0 until 64).map { p =>
      val y = p / 8
      val x = p % 8
      this (x, y).hashCode().toLong << p
    }.sum
    (long ^ (long >>> 32)).toInt
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case b: SimpleBoard => hashCode == b.hashCode && equalFigures(b)
      case _ => ???
    }
  }
}

class SimpleBoardBuilder() extends BoardBuilder {
  val array: Array[Figure] = (0 until 64).map(i => Figure.Empty).toArray

  override def board: Board = new SimpleBoard(1, GamePlaying, None, array.clone())

  override def add(boardActions: Seq[(Figure, Position)]): BoardBuilder = {
    boardActions.foreach {
      case (fig, pos) =>
        array(pos.x + pos.y * 8) = fig
    }
    this
  }
}

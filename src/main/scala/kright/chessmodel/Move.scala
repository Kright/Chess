package kright.chessmodel

import kright.chessmodel.Figure._

/**
  * Created by lgor on 6/8/17.
  */
sealed trait Move {
  def makeDescription(b: Board): String

  def apply(b: Board): Board

  def isValid(b: Board): Boolean
}

object Move {

  def parse(b: Board, s: String): Option[Move] = {
    val simpleMove = "[abcdefgh][12345678][\\-x][abcdefgh][12345678]"

    if (s.matches(simpleMove)) {
      for (from <- Position.parse(s.substring(0, 2));
           to <- Position.parse(s.substring(3, 3 + 2))) {
        return Option(SimpleMove(from, to))
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

    None
  }
}

//todo рокировку, взятие на проходе

case class SimpleMove(from: Position, to: Position) extends Move {
  def makeDescription(b: Board): String = {
    val f1 = b(from)
    val f2 = b(to)
    val symbol = if (f2 == Figure.Empty) "-" else "x"
    s"${f1.shortName}$from$symbol$to"
  }

  override def apply(b: Board): Board = {
    val figure = b(from)
    b.doTurn(Figure.Empty -> from, figure -> to)
  }

  override def isValid(b: Board): Boolean = {
    val f1 = b(from)
    if (!f1.isPlayerFig(b.currentPlayer)) return false
    val f2 = b(to)
    if (f2.isPlayerFig(b.currentPlayer)) return false

    f1.possibleMoves(b, from.x, from.y).contains(this)
  }
}


case class Pawn2Fig(from: Position, to: Position, figure: Figure) extends Move {
  assert(figure != Figure.Empty)
  assert(!figure.isInstanceOf[Figure.Pawn])

  override def apply(b: Board): Board = {
    b.doTurn(Figure.Empty -> from, figure -> to)
  }

  override def makeDescription(b: Board): String = {
    val f1 = b(from)
    val f2 = b(to)
    val symbol = if (f2 == Figure.Empty) "-" else "x"
    s"${f1.shortName}$from$symbol$to${figure.shortName}"
  }

  override def isValid(b: Board): Boolean = {
    val f1 = b(from)

    if (!(f1.isPlayerFig(b.currentPlayer) && f1.isPawn)) return false

    f1.possibleMoves(b, from.x, from.y).contains(this)
  }
}

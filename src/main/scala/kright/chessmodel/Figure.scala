package kright.chessmodel

import kright._
import kright.chessmodel.Figure._

import scala.collection.mutable.ArrayBuffer

/**
  * Created by lgor on 6/6/17.
  */
sealed trait Figure {

  def isEmpty: Boolean

  def isPlayerFig(player: Int): Boolean


  def possibleMovesCount(board: Board, x: Int, y: Int): Int

  def addPossibleMoves(board: Board, x: Int, y: Int, acc: Figure.MovesAccum): Unit


  def isKing: Boolean = false

  def isQueen: Boolean = false

  def isPawn: Boolean = false

  def isRook: Boolean = false

  def isKnight: Boolean = false

  def isBishop: Boolean = false


  def shortName: String =
    this match {
      case Figure.Empty => "_"
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

  override def isEmpty = false

  override def isPlayerFig(p: Int): Boolean = p == player

  protected def canMoveTo(b: Board, x: Int, y: Int): Boolean = Position.isValid(x, y) && !b(x, y).isPlayerFig(player)
}


trait LinearFig extends PlayerFig {
  protected def addLinearMoves(dx: Int, dy: Int, board: Board, pos: Position, arr: Figure.MovesAccum): Unit = {
    for (i <- 1 to 7) {
      val x = pos.x + dx * i
      val y = pos.y + dy * i

      if (!canMoveTo(board, x, y)) return

      arr += SimpleMove(pos, new Position(x, y))
      if (board(x, y) != Figure.Empty)
        return
    }
  }

  protected def linearMovesCount(dx: Int, dy: Int, board: Board, posX: Int, posY: Int): Int = {
    var count = 0

    for (i <- 1 to 7) {
      val x = posX + dx * i
      val y = posY + dy * i

      if (!canMoveTo(board, x, y)) return count
      count += 1

      if (!board(x, y).isEmpty)
        return count
    }

    count
  }
}

trait RookMoves extends LinearFig {
  def addRockMoves(b: Board, position: Position, arr: Figure.MovesAccum): Unit = {
    addLinearMoves(1, 0, b, position, arr)
    addLinearMoves(-1, 0, b, position, arr)
    addLinearMoves(0, -1, b, position, arr)
    addLinearMoves(0, 1, b, position, arr)
  }

  def rockMovesCount(b: Board, x: Int, y: Int): Int = {
    linearMovesCount(1, 0, b, x, y) +
      linearMovesCount(-1, 0, b, x, y) +
      linearMovesCount(0, -1, b, x, y) +
      linearMovesCount(0, 1, b, x, y)
  }
}

trait BishopMoves extends LinearFig {
  def addBishopMoves(b: Board, position: Position, arr: Figure.MovesAccum): Unit = {
    addLinearMoves(-1, -1, b, position, arr)
    addLinearMoves(-1, 1, b, position, arr)
    addLinearMoves(1, -1, b, position, arr)
    addLinearMoves(1, 1, b, position, arr)
  }

  def bishopMovesCount(b: Board, x: Int, y: Int): Int = {
    linearMovesCount(-1, -1, b, x, y) +
      linearMovesCount(-1, 1, b, x, y) +
      linearMovesCount(1, -1, b, x, y) +
      linearMovesCount(1, 1, b, x, y)
  }
}

trait KKmoves extends PlayerFig {

  def tryAddMove(x: Int, y: Int, from: Position, board: Board, arr: Figure.MovesAccum): Unit = {
    if (canMoveTo(board, x, y))
      arr += SimpleMove(from, new Position(x, y))
  }

  def canAddMove(x: Int, y: Int, board: Board): Int = if (canMoveTo(board, x, y)) 1 else 0
}


object Figure {

  type MovesAccum = ArrayBuffer[Move]

  object Empty extends Figure {
    override def isPlayerFig(player: Int): Boolean = false

    override def isEmpty: Boolean = true

    override def possibleMovesCount(board: Board, x: Int, y: Int): Int = 0

    override def addPossibleMoves(board: Board, x: Int, y: Int, acc: MovesAccum): Unit = {
      //Nothing
    }

    override def hashCode(): Int = 0
  }

  case class King(player: Int) extends KKmoves {

    override def isKing: Boolean = true

    override def hashCode(): Int = if (player == 1) 'k' else -'k'

    override def addPossibleMoves(b: Board, x: Int, y: Int, arr: MovesAccum): Unit = {

      val pos = new Position(x, y)

      tryAddMove(x - 1, y - 1, pos, b, arr)
      tryAddMove(x - 1, y, pos, b, arr)
      tryAddMove(x - 1, y + 1, pos, b, arr)

      tryAddMove(x, y - 1, pos, b, arr)
      tryAddMove(x, y + 1, pos, b, arr)

      tryAddMove(x + 1, y - 1, pos, b, arr)
      tryAddMove(x + 1, y, pos, b, arr)
      tryAddMove(x + 1, y + 1, pos, b, arr)
    }

    override def possibleMovesCount(b: Board, x: Int, y: Int): Int =
      canAddMove(x - 1, y - 1, b) +
        canAddMove(x - 1, y, b) +
        canAddMove(x - 1, y + 1, b) +
        canAddMove(x, y - 1, b) +
        canAddMove(x, y + 1, b) +
        canAddMove(x + 1, y - 1, b) +
        canAddMove(x + 1, y, b) +
        canAddMove(x + 1, y + 1, b)
  }

  case class Bishop(player: Int) extends BishopMoves {

    override def isBishop: Boolean = true

    override def hashCode(): Int = if (player == 1) 'b' else -'b'

    override def addPossibleMoves(b: Board, x: Int, y: Int, arr: Figure.MovesAccum): Unit =
      addBishopMoves(b, new Position(x, y), arr)

    override def possibleMovesCount(b: Board, x: Int, y: Int): Int =
      bishopMovesCount(b, x, y)
  }

  case class Rook(player: Int) extends RookMoves {

    override def isRook: Boolean = true

    override def hashCode(): Int = if (player == 1) 'r' else -'r'

    override def addPossibleMoves(b: Board, x: Int, y: Int, arr: Figure.MovesAccum): Unit =
      addRockMoves(b, new Position(x, y), arr)

    override def possibleMovesCount(b: Board, x: Int, y: Int): Int =
      rockMovesCount(b, x, y)
  }

  case class Queen(player: Int) extends RookMoves with BishopMoves {
    override def isQueen: Boolean = true

    override def hashCode(): Int = if (player == 1) 'q' else -'q'

    override def addPossibleMoves(b: Board, x: Int, y: Int, arr: Figure.MovesAccum): Unit = {
      val pos = new Position(x, y)
      addRockMoves(b, pos, arr)
      addBishopMoves(b, pos, arr)
    }

    override def possibleMovesCount(b: Board, x: Int, y: Int): Int = rockMovesCount(b, x, y) + bishopMovesCount(b, x, y)
  }

  case class Knight(player: Int) extends KKmoves {
    override def isKnight: Boolean = true

    override def hashCode(): Int = if (player == 1) 'n' else -'n'

    override def addPossibleMoves(b: Board, x: Int, y: Int, arr: Figure.MovesAccum): Unit = {
      val pos = new Position(x, y)

      tryAddMove(x - 2, y - 1, pos, b, arr)
      tryAddMove(x - 2, y + 1, pos, b, arr)
      tryAddMove(x + 2, y - 1, pos, b, arr)
      tryAddMove(x + 2, y + 1, pos, b, arr)

      tryAddMove(x - 1, y - 2, pos, b, arr)
      tryAddMove(x + 1, y - 2, pos, b, arr)
      tryAddMove(x - 1, y + 2, pos, b, arr)
      tryAddMove(x + 1, y + 2, pos, b, arr)
    }

    override def possibleMovesCount(b: Board, x: Int, y: Int): Int =
      canAddMove(x - 2, y - 1, b) +
        canAddMove(x - 2, y + 1, b) +
        canAddMove(x + 2, y - 1, b) +
        canAddMove(x + 2, y + 1, b) +
        canAddMove(x - 1, y - 2, b) +
        canAddMove(x + 1, y - 2, b) +
        canAddMove(x - 1, y + 2, b) +
        canAddMove(x + 1, y + 2, b)
  }

  case class Pawn(player: Int) extends PlayerFig {
    override def hashCode(): Int = if (player == 1) 'p' else -'p'

    override def isPawn: Boolean = true

    override def addPossibleMoves(board: Board, posX: Int, posY: Int, arr: Figure.MovesAccum): Unit = {
      val dy = board.currentPlayer
      val pos = new Position(posX, posY)
      val forward = new Position(posX, posY + dy)
      assert(forward.isValid)

      //moving forward
      if (board(forward).isEmpty) {
        arr ++= move(pos, forward)

        if (initialPos(posY) && board(posX, posY + dy * 2).isEmpty) {
          arr ++= move(pos, new Position(posX, posY + dy * 2))
        }
      }

      //eating
      if (Position.isValid(pos.x + 1, pos.y + dy) && board(pos.x + 1, pos.y + dy).isPlayerFig(-player)) {
        arr ++= move(pos, new Position(pos.x + 1, pos.y + dy))
      }

      if (Position.isValid(pos.x - 1, pos.y + dy) && board(pos.x - 1, pos.y + dy).isPlayerFig(-player)) {
        arr ++= move(pos, new Position(pos.x - 1, pos.y + dy))
      }
    }

    private def initialPos(y: Int): Boolean = (y == 1 && player == 1) || (y == 6 && player == -1)

    private def maxLine(p: Position): Boolean = (p.y == 7 && player == 1) || (p.y == 0 && player == -1)

    private def move(from: Position, to: Position): Seq[Move] =
      if (maxLine(to))
        Seq(Queen(player), Bishop(player), Knight(player), Rook(player)).map(Pawn2Fig(from, to, _))
      else
        Seq(SimpleMove(from, to))

    override def possibleMovesCount(board: Board, posX: Int, posY: Int): Int = {
      val dy = if (board(posX, posY).isPlayerFig(1)) 1 else -1
      val forward = new Position(posX, posY + dy)
      assert(forward.isValid)

      var summ = 0

      //moving forward
      if (board(forward).isEmpty) {
        summ += 1

        if (initialPos(posY) && board(posX, posY + dy * 2).isEmpty)
          summ += 1
      }

      //eating
      if (Position.isValid(posX + 1, posY + dy) && board(posX + 1, posY + dy).isPlayerFig(-player))
        summ += 1

      if (Position.isValid(posX - 1, posY + dy) && board(posX - 1, posY + dy).isPlayerFig(-player))
        summ += 1

      summ
    }
  }

}

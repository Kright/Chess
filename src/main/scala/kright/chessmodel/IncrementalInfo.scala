package kright.chessmodel

import kright.chessmodel.Board.BoardAction
import kright.chessmodel.Figure._

/**
  * Created by lgor on 6/11/17.
  */
object IncrementalInfo {

  private def buildFromNull(b: Board): IncrementalInfo = {
    val info = new IncrementalInfo()

    for (y <- 0 until 8;
         x <- 0 until 8) {
      b(x, y) match {
        case Figure.Empty =>
        case Pawn(p) =>
          info(p).pawns += 1
          info(p).pawnsOnLines(x) += 1
        case Rook(p) => info(p).rooks += 1
        case Knight(p) => info(p).knights += 1
        case Bishop(p) => info(p).bishops += 1
        case Queen(p) => info(p).queens += 1
        case King(p) =>
      }
    }

    info
  }

  private def buildIncremental(before: Board, actions: Seq[BoardAction], after: Board): IncrementalInfo = {
    val info = before.info.copy()

    for (act <- actions) {
      val pos = act._2

      before(pos) match {
        case Figure.Empty =>
        case Pawn(p) =>
          info(p).pawns -= 1
          info(p).pawnsOnLines(pos.x) -= 1
        case Rook(p) => info(p).rooks -= 1
        case Knight(p) => info(p).knights -= 1
        case Bishop(p) => info(p).bishops -= 1
        case Queen(p) => info(p).queens -= 1
        case King(p) =>
      }

      act._1 match {
        case Figure.Empty =>
        case Pawn(p) =>
          info(p).pawns += 1
          info(p).pawnsOnLines(pos.x) += 1
        case Rook(p) => info(p).rooks += 1
        case Knight(p) => info(p).knights += 1
        case Bishop(p) => info(p).bishops += 1
        case Queen(p) => info(p).queens += 1
        case King(p) =>
      }
    }

    info
  }

  def apply(board: Board) =
    board.previous.map { case (before, actions) =>
      buildIncremental(before, actions, board)
    }.getOrElse {
      buildFromNull(board)
    }
}


class IncrementalInfo(val white: PlayerSensitiveInfo,
                      val black: PlayerSensitiveInfo) {

  def this() = this(new PlayerSensitiveInfo(), new PlayerSensitiveInfo())

  def copy() = new IncrementalInfo(white.copy(), black.copy())

  def apply(player: Int): PlayerSensitiveInfo = player match {
    case 1 => white
    case -1 => black
  }
}


class PlayerSensitiveInfo(var pawns: Int,
                          var rooks: Int,
                          var knights: Int,
                          var bishops: Int,
                          var queens: Int,
                          val pawnsOnLines: Array[Int]) {

  def this() = this(0, 0, 0, 0, 0, new Array[Int](8))

  def copy() = new PlayerSensitiveInfo(pawns, rooks, knights, bishops, queens, pawnsOnLines.clone())
}

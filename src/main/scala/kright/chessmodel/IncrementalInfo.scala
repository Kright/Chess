package kright.chessmodel

import kright.chessmodel.Board.BoardAction
import kright.chessmodel.figure._

/**
	* Created by lgor on 6/11/17.
	*/
object IncrementalInfo {

	def apply(board: Board): IncrementalInfo =
		board.previous.map { case (before, actions) =>
			buildIncremental(before, actions, board)
		}.getOrElse {
			buildFromNull(board)
		}

	private def buildFromNull(board: Board): IncrementalInfo = {
		val info = new IncrementalInfo()

		for (y <- 0 until 8;
		     x <- 0 until 8) {
			board(x, y) match {
				case Empty =>
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

		def isPlayerRook(x: Int, y: Int, player: Int) = board(x, y).isRook && board(x, y).isPlayerFig(player)

		for (player <- Seq(-1, 1)) {
			val y = if (player == 1) 0 else 7

			info(player).rookAWasMoved = !isPlayerRook(0, y, player)
			info(player).rookHWasMoved = !isPlayerRook(7, y, player)

			info(player).kingWasMoved = !(board(4, y).isKing && board(4, y).isPlayerFig(player))
		}

		info
	}

	private def buildIncremental(before: Board, actions: Seq[BoardAction], after: Board): IncrementalInfo = {
		val info = before.info.copy()

		for (act <- actions) {
			val pos = act._2

			before(pos) match {
				case Empty =>
				case Pawn(p) =>
					info(p).pawns -= 1
					info(p).pawnsOnLines(pos.x) -= 1
				case Rook(p) =>
					info(p).rooks -= 1
					val initialY = if (p == 1) 0 else 7
					if (pos.y == initialY) {
						pos.x match {
							case 0 => info(p).rookAWasMoved = true
							case 7 => info(p).rookHWasMoved = true
							case _ =>
						}
					}
				case Knight(p) => info(p).knights -= 1
				case Bishop(p) => info(p).bishops -= 1
				case Queen(p) => info(p).queens -= 1
				case King(p) => info(p).kingWasMoved = true
			}

			act._1 match {
				case Empty =>
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
                          val pawnsOnLines: Array[Int],
                          var rookAWasMoved: Boolean,
                          var rookHWasMoved: Boolean,
                          var kingWasMoved: Boolean) {

	def this() = this(0, 0, 0, 0, 0, new Array[Int](8), false, false, false)

	def copy() = new PlayerSensitiveInfo(pawns, rooks, knights, bishops, queens, pawnsOnLines.clone(), rookAWasMoved, rookHWasMoved, kingWasMoved)

	def castlingAPossible: Boolean = !kingWasMoved && !rookAWasMoved

	def castlingHPossible: Boolean = !kingWasMoved && !rookHWasMoved
}

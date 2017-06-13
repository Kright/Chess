package kright.chessmodel

import kright.chessmodel.Board.BoardAction
import kright.chessmodel.figure._

/**
	* Created by lgor on 6/6/17.
	*/
trait Board {
	def currentPlayer: Int

	def apply(x: Int, y: Int): Figure

	def doTurn(actions: BoardAction*): Board

	def gameStatus: GameStatus

	def previous: Option[(Board, Seq[BoardAction])]


	private var _info: IncrementalInfo = _

	def info: IncrementalInfo = {
		if (_info == null)
			_info = IncrementalInfo(this)
		_info
	}

	override def hashCode(): Int

	override def equals(obj: Any): Boolean


	def apply(p: Position): Figure = apply(p.x, p.y)

	def safe(x: Int, y: Int): Figure =
		if (Position.isValid(x, y))
			this (x, y)
		else
			figure.Empty


	def allPossibleMoves(): Figure.MovesAccum = {
		val buf = new Figure.MovesAccum()

		if (gameStatus.gameFinished) return buf

		for (y <- 0 until 8;
		     x <- 0 until 8) {
			val fig = this (x, y)

			if (fig.isPlayerFig(currentPlayer)) {
				fig.addPossibleMoves(this, x, y, buf)
			}
		}

		buf
	}

	final def equalFigures(b: Board): Boolean =
		(0 until 64).forall { p =>
			val y = p / 8
			val x = p % 8
			this (x, y) == b(x, y)
		}

	private def makeLine(y: Int) = (0 until 8).map(x => this (x, y).playerCaseName).mkString(" ")

	def consolePicture: String = {
		s"Current player : ${if (this.currentPlayer == 1) "white" else "black"}, ${this.gameStatus}\n" +
			"  a b c d e f g h\n" +
			(7 to 0 by -1).map(y => s"${y + 1} ${makeLine(y)}").mkString("\n")
	}
}

object Board {
	type BoardAction = (Figure, Position)

	val kings: Seq[BoardAction] = Seq(King(1) -> new Position(4, 0), King(-1) -> new Position(4, 7))

	val queens: Seq[BoardAction] = Seq(Queen(1) -> new Position(3, 0), Queen(-1) -> new Position(3, 7))

	val pawns: Seq[BoardAction] = (0 until 8).flatMap(x => List(Pawn(1) -> new Position(x, 1), Pawn(-1) -> new Position(x, 6)))

	val rooks: Seq[BoardAction] = addSymm(Rook.apply, 0, 0)

	val knights: Seq[BoardAction] = addSymm(Knight.apply, 1, 0)

	val bishops: Seq[BoardAction] = addSymm(Bishop.apply, 2, 0)

	def allFigures: Seq[BoardAction] = pawns ++ kings ++ queens ++ rooks ++ bishops ++ knights

	private def addSymm(c: (Int) => PlayerFig, x: Int, y: Int): Seq[BoardAction] = {
		val f = c(1)
		val f2 = c(-1)
		Seq(f -> new Position(x, y), f -> new Position(7 - x, y), f2 -> new Position(x, 7 - y), f2 -> new Position(7 - x, 7 - y))
	}
}

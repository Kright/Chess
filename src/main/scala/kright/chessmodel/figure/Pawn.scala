package kright.chessmodel.figure

import kright.chessmodel._
import kright.chessmodel.move.{Move, Pawn2Fig, PawnEatInPass, SimpleMove}

/**
	* Created by lgor on 13.06.2017.
	*/
case class Pawn(player: Int) extends PlayerFig {
	override def hashCode(): Int = if (player == 1) 'p' else -'p'

	override def isPawn: Boolean = true

	override def addPossibleMoves(board: Board, posX: Int, posY: Int, arr: Figure.MovesAccum): Unit = {
		val dy = board.currentPlayer
		val pos = Position(posX, posY)
		val forward = Position(posX, posY + dy)
		assert(forward.isValid)

		//moving forward
		if (board(forward).isEmpty) {
			arr ++= move(pos, forward)

			if (initialPos(posY) && board(posX, posY + dy * 2).isEmpty) {
				arr ++= move(pos, Position(posX, posY + dy * 2))
			}
		}

		//eating
		if (Position.isValid(pos.x + 1, pos.y + dy) && board(pos.x + 1, pos.y + dy).isPlayerFig(-player)) {
			arr ++= move(pos, Position(pos.x + 1, pos.y + dy))
		}

		if (Position.isValid(pos.x - 1, pos.y + dy) && board(pos.x - 1, pos.y + dy).isPlayerFig(-player)) {
			arr ++= move(pos, Position(pos.x - 1, pos.y + dy))
		}

		{
			val dir = player
			val curY = if (player == 1) 4 else 3
			if (pos.y == curY) {
				{
					val left = board.safe(pos.x - 1, curY)
					if (left.isPawn && left.isPlayerFig(-player) && board(pos.x - 1, curY + 2 * dir).isEmpty) {
						board.previous.foreach {
							case (prevBoard, _) =>
								val prev = prevBoard(pos.x - 1, curY + 2 * dir)
								if (prevBoard(pos.x - 1, curY).isEmpty && prev.isPawn && prev.isPlayerFig(-player)) {
									arr += PawnEatInPass(
										from = pos,
										to = new Position(pos.x - 1, curY + dir),
										eated = new Position(pos.x - 1, curY))
								}
						}
					}
				}

				val right = board.safe(pos.x + 1, curY)
				if (right.isPawn && right.isPlayerFig(-player) && board(pos.x + 1, curY + 2 * dir).isEmpty) {
					board.previous.foreach {
						case (prevBoard, _) =>
							val prev = prevBoard(pos.x + 1, curY + 2 * dir)
							if (prevBoard(pos.x + 1, curY).isEmpty && prev.isPawn && prev.isPlayerFig(-player)) {
								arr += PawnEatInPass(
									from = pos,
									to = new Position(pos.x + 1, curY + 1),
									eated = new Position(pos.x + 1, curY))
							}
					}
				}
			}
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

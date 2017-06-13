package kright

import kright.chessmodel.Board

/**
	* Created by lgor on 6/8/17.
	*/
class Game(val white: Player, val black: Player) {

	def play(b: Board): Board = {
		var board = b

		var currentTurn = 0

		def nextPlayer(): Player = {
			currentTurn += 1
			if (currentTurn % 2 == 1) white else black
		}

		while (board.gameStatus.playing) {
			val player = nextPlayer()
			val move = player.doTurn(board)
			assert(move.isValid(board), s"invalid move : $move")
			board = move(board)
		}

		board
	}
}

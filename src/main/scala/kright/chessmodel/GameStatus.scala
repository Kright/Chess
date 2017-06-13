package kright.chessmodel

/**
	* Created by lgor on 6/9/17.
	*/
sealed trait GameStatus {

	def gameFinished: Boolean

	def playing: Boolean = !gameFinished
}

case object GamePlaying extends GameStatus {
	override def gameFinished: Boolean = false

	override def toString: String = "game is playing"
}

case class GameFinished(winner: Int) extends GameStatus {
	assert(math.abs(winner) <= 1)

	override def gameFinished: Boolean = true

	override def toString: String =
		winner match {
			case 1 => "white is won"
			case -1 => "black is won"
			case 0 => "draw"
		}
}

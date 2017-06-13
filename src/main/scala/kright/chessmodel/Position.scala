package kright.chessmodel

import scala.math.{abs, max}

/**
	* Created by lgor on 6/8/17.
	*/
case class Position(x: Int, y: Int) {

	def isValid: Boolean = Position.isValid(x, y)

	def distanceDiag(p: Position): Int = max(abs(x - p.x), abs(y - p.y))

	def distanceL1(p: Position): Int = abs(x - p.x) + abs(y - p.y)

	override def toString: String = s"${('a' + x).toChar}${y + 1}"
}

object Position {

	def parse(substr: String): Option[Position] = {
		if (substr.size != 2) return None
		val pos = new Position(substr(0) - 'a', substr(1) - '1')
		if (pos.isValid) Option(pos) else None
	}

	def isValid(x: Int, y: Int): Boolean = isValid(x) && isValid(y)

	def isValid(x: Int): Boolean = x >= 0 && x <= 7

	def isCenter(x: Int, y: Int): Boolean = isCenter(x) && isCenter(y)

	def isCenter(x: Int): Boolean = x == 3 || x == 4
}

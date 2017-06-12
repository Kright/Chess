package kright

import kright.chessmodel.Board

/**
  * Created by lgor on 6/10/17.
  */
object Implicits {

  implicit class BoardWithDescription(val b: Board) extends AnyVal {
    private def makeLine(y: Int) = (0 until 8).map(x => b(x, y).playerCaseName).mkString(" ")

    def consolePicture: String = {
      s"Current player : ${if (b.currentPlayer == 1) "white" else "black"}, ${b.gameStatus}\n" +
        "  a b c d e f g h\n" +
        (7 to 0 by -1).map(y => s"${y + 1} ${makeLine(y)}").mkString("\n")
    }
  }

}

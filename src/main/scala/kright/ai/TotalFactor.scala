package kright.ai

import kright.chessmodel.{Board, GameFinished}

/**
  * Created by lgor on 6/11/17.
  */
class TotalFactor(val arr: Array[(Double, WinFactor)]) extends WinFactor {

  def this(seq: (Double, WinFactor)*) = this(seq.toArray)

  override def apply(b: Board): Double = {

    if (b.gameStatus.gameFinished) {
      return b.gameStatus match {
        case GameFinished(winner) =>
          winner match {
            case 1 => 1
            case -1 => 0
            case 0 => 0.5f
          }
        case _ => ???
      }
    }

    val balance: Double = arr.map { case (weight, factor) => weight * factor(b) }.sum

    // https://habrahabr.ru/post/254753/
    val probWhiteWins = 1.0 / (1.0 + math.exp(-0.7 * balance))

    probWhiteWins
  }
}

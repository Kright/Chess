package kright.ai

import kright.chessmodel._

/**
  * Created by lgor on 6/8/17.
  */
trait WinFactor {

  def apply(b: Board): Double

  def apply(b: Board, p: Int): Double =
    p match {
      case 1 => apply(b)
      case -1 => 1 - apply(b)
    }
}

object WinFactor {
  def simplest: WinFactor = new TotalFactor(1.0 -> new FiguresWeightFactor())

  def withPosition: WinFactor = new TotalFactor(1.0 -> new FiguresWeightFactor(), 0.01 -> new PositionFactor())
}










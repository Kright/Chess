package kright.ai

import kright.chessmodel.{Board, Move}

/**
  * Created by lgor on 6/11/17.
  */
trait Walker {
  def doTurn(board: Board, metrics: WinFactor): Move
}

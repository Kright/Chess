package kright.ai

import kright.chessmodel.{Board, GameFinished, GamePlaying, Move}

import scala.collection.mutable

/**
  * Created by lgor on 6/11/17.
  */
class CachedWalker(val recursionDepth: Int, val cacheDepth: Int) extends Walker {
  assert(recursionDepth > 0)
  assert(recursionDepth >= cacheDepth)

  override def doTurn(board: Board, metrics: WinFactor): Move = {
    val start = System.currentTimeMillis()
    val session = new WalkingSession(metrics, recursionDepth - cacheDepth)

    val moves = board.allPossibleMoves()
    assert(moves.nonEmpty)

    val best = moves.map { move =>
      (move, session.findProb(move(board), recursionDepth - 1))
    }.maxBy(_._2.prob * board.currentPlayer)._1

    val dt = System.currentTimeMillis() - start
    println(s"True cache $recursionDepth, hits: ${session.cacheHit}, misses : ${session.cacheMiss}, time ${dt}ms, perf = ${session.cacheMiss.toLong * 1000 / dt} pos/sec")

    best
  }
}

class WalkingSession(metrics: WinFactor, minLevel: Int = 0) {

  val cache = new mutable.HashMap[Board, WinProbability]
  var cacheMiss = 0
  var cacheHit = 0

  def computeProb(board: Board, level: Int): WinProbability = {
    val prob = board.gameStatus match {
      case GamePlaying =>
        if (level <= 0) {
          WinProbability(metrics(board), 0)
        } else {
          val moves = board.allPossibleMoves()
          assert(moves.nonEmpty)
          val best = moves.map(move => (move, findProb(move(board), level - 1))).maxBy(_._2.prob * board.currentPlayer)
          WinProbability(best._2.prob, level)
        }
      case GameFinished(winner) =>
        WinProbability((winner + 1.0) * 0.5, 80)
    }

    if (minLevel <= prob.level)
      cache(board) = prob
    prob
  }

  def findProb(b: Board, level: Int): WinProbability = {
    cache.get(b) match {
      case Some(winProb) =>
        if (winProb.level >= level) {
          cacheHit += 1
          winProb
        }
        else {
          cacheMiss += 1
          computeProb(b, level)
        }
      case None =>
        cacheMiss += 1
        computeProb(b, level)
    }
  }
}

case class WinProbability(prob: Double, level: Int)

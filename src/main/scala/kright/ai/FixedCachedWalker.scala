package kright.ai

import kright.Player
import kright.chessmodel.{Board, Move}

import scala.collection.mutable

/**
  * Created by lgor on 6/10/17.
  */
class FixedCachedWalker(val recursionDepth: Int = 3) extends Walker {

  var cacheMiss = 0
  var cacheHit = 0

  override def doTurn(board: Board, metrics: WinFactor): Move = {
    val t = System.currentTimeMillis()

    val cache = new mutable.HashMap[Board, Double]
    cacheMiss = 0
    cacheHit = 0

    def prob(board: Board, depth: Int): Double = {
      cache.get(board) match {
        case Some(p) =>
          cacheHit += 1
          p
        case None =>
          cacheMiss += 1
          if (depth <= 0) {
            metrics(board)
          } else {
            val moves = board.allPossibleMoves()
            if (moves.isEmpty) {
              if (Player.isWhite(board.currentPlayer)) 0 else 1
            } else {
              val p = moves.map(move => (move, prob(move(board), depth - 1))).maxBy(_._2 * board.currentPlayer)._2
              cache(board) = p
              p
            }
          }
      }
    }

    val m = board.allPossibleMoves().map(move => (move, prob(move(board), recursionDepth - 1))).maxBy(_._2 * board.currentPlayer)._1
    val dt = System.currentTimeMillis() - t
    println(s"cache hits: $cacheHit, misses : $cacheMiss, time ${dt}ms, perf = ${cacheMiss.toLong * 1000 / dt} pos/sec")
    m
  }
}





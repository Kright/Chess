package kright

import kright.ai._
import kright.chessmodel.{Board, BoardBuilder}

/**
  * Created by lgor on 6/6/17.
  */
object Main extends App {

  def time(func: => Unit): Long = {
    val t = System.currentTimeMillis()
    func
    val t2 = System.currentTimeMillis()
    t2 - t
  }

  def stats(): Unit = {
    val board = new SimpleBoardBuilder().add(Board.allFigures).board

    val walkers = (2 to 6).flatMap(i => Seq(
      (new FixedCachedWalker(i), s"depth = $i"),
      (new CachedWalker(i, i - 1), s"cached depth = $i")))

    val factors = Seq(
      (WinFactor.simplest, "simplest"),
      (WinFactor.withPosition, "withPos")
    )

    time {
      for (w <- walkers; f <- factors) {
        println()
        val t = time {
          println(AI(w._1, f._1).doTurn(board))
        }
        println(s"${t}ms\t${w._2}\t${f._2}")
      }
    }
  }

  def pvp(): Unit = {
    val white = AI(new FixedCachedWalker(5), WinFactor.simplest)
    val black = AI(new FixedCachedWalker(4), WinFactor.simplest)

    val theGame = new Game(new PrintingBoardPlayer(white), new PrintingBoardPlayer(black))

    theGame.play(BoardBuilder.standard)
  }

  stats()
}

/*
ле пиздец, всё сложно и неправильно.

Итак, можно просто ходить (и есть)
пешки на последней линии становятся любой фигурой
пешки можно есть другой пешкой "на проходе", но только сразу после её хода

для простейшей функции оценки и фиксированного перебора
120кк вариантов - 354с
если юзать кеширование - 49с (и уникальный вариантов только 6.9кк)


скорость перебора до добавления incrementalInfo:
веса фигур = 260k/sec
pos+веса = 165k/sec

после добавления:
веса фигур = 230k/sec
использование info: 370k/sec
pos+веса = 150k/sec
pos + веса из info = 180k/sec

добавил счётчик пешек на линии:
веса = 350к/sec
pos + веса = 210k/sec

 */

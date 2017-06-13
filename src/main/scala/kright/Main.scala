package kright

import kright.ai._
import kright.chessmodel.{Board, BoardBuilder, SimpleBoardBuilder}
import kright.ai.walker.{MinMaxClever, MinMaxReordered, MinMaxWalker}

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

		val walkers = (2 to 5).flatMap(i => Seq(
			//(new FixedDepthWalker(i), s"FixedDepthWalker($i)"),
			(new MinMaxWalker(i), s"MinMaxWalker($i)"),
			(new MinMaxReordered(i), s"MinMaxReordered($i)"),
			(new MinMaxClever(i), s"MinMaxClever($i)")
		))

		val factors = Seq(
			//(WinFactor.simplest, "simplest"),
			(WinFactor.withPosition, "withPos")
		)

		for (w <- walkers; f <- factors) {
			println()
			val t = time {
				println(AI(w._1, f._1).doTurn(board))
			}
			println(s"${t}ms\t${w._2}\t${f._2}")
		}
	}

	def AIvsAI(): Unit = {
		val white = AI(new MinMaxReordered(5), WinFactor.withPosition)
		val black = AI(new MinMaxClever(4), WinFactor.withPosition)

		val theGame = new Game(PrintingBoardPlayer(white), PrintingBoardPlayer(black))

		theGame.play(BoardBuilder.standard)
	}

	def pve(): Unit = {
		val black = AI(new MinMaxClever(4, 0.5), WinFactor.withPosition)
		val me = new ReplPlayer

		val game = new Game(PrintingBoardPlayer(me), PrintingBoardPlayer(black))

		println(BoardBuilder.standard.board.consolePicture)

		game.play(BoardBuilder.standard)
	}

	def castling(): Unit = {
		val board = BoardBuilder.empty.add(Board.kings).add(Board.rooks)

		val game = new Game(PrintingBoardPlayer(new ReplPlayer), PrintingBoardPlayer(new ReplPlayer))
		game.play(board)
	}

	//AIvsAI()
	//stats()
	//castling()
	pve()
}

/*
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


12 июня

реализовал min-max алгоритм и сравнил (правда, на другом ноуте)
функция оценки по весам фигур даёт не очень большой эффект, но более продвинутая хорошо работает

depth 2
400 vs 237

depth 3 x3
8902 vs 2992

depth 4 x6
197742, 480ms vs 33924, 150ms

depth 5: x10
4,896998, 23041ms vs 431931, 2150ms

depth 6: x30
120,897438 629509ms vs 4,969924, 32195ms
 */

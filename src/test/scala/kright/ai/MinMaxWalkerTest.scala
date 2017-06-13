package kright.ai

import kright.ai.walker.{FixedDepthWalker, MinMaxWalker}
import kright.chessmodel.{Board, BoardBuilder}
import org.scalatest.FunSuite

/**
	* Created by lgor on 12.06.2017.
	*/
class MinMaxWalkerTest extends FunSuite {

	test("resultsAreEqualsWithMinMax") {
		val board: Board = BoardBuilder.standard

		for (depth <- 1 to 4;
		     f <- Seq(WinFactor.simplest, WinFactor.withPosition)) {
			val minMax = AI(new MinMaxWalker(3), f)
			val simplest = AI(new FixedDepthWalker(3), f)

			assert(minMax.doTurn(board) == simplest.doTurn(board))
		}
	}

}

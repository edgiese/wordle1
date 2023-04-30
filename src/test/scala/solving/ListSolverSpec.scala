package com.edgiese.wordle1
package solving
import judging.Judge

import org.scalatest.flatspec.AnyFlatSpec

class ListSolverSpec extends AnyFlatSpec:
  behavior of "a list solver"

  private val wordSet1 = List("apple", "arise", "spine", "spite", "mover", "topaz")
  private val baseConfig = ListSolverConfig()
  private val listSolver = ListSolver(wordSet1, wordSet1, baseConfig)

  it should "make the right guess when only one works" in {
    val game = for
      g1 <- Game(5, 6, Judge("apple", false, wordSet1))
      g2 <- g1.addGuess("topaz")
    yield g2
    listSolver.solve(game.toOption.get) match
      case Right(solution) =>
        assert(solution.word == "apple")
        assert(solution.oneOf == 1)
      case Left(error) => fail("Solve error: " + error)
  }


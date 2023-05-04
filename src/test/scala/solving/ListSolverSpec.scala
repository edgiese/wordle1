package com.edgiese.wordle1
package solving
import judging.Judge

import org.scalatest.flatspec.AnyFlatSpec

class ListSolverSpec extends AnyFlatSpec:
  behavior of "a list solver"

  private val wordSet1 = List("apple", "arise", "spine", "spite", "mover", "topaz")
  private val baseConfig = ListSolverConfig()
  private val listSolver = ListSolver(wordSet1, wordSet1, baseConfig)

  it should "return an error when the game is over" in {
    val game = for
      g1 <- Game(5, 6, Judge("apple", false, wordSet1))
      g2 <- g1.addGuess("apple")
    yield g2
    listSolver.solve(game.toOption.get) match
      case Right(_) => fail("should return an error but did not")
      case Left(error) => assert(error == SolverError.GameOver)
  }

  it should "resign when there are no more words to try" in {
    val smallWordSet = List("apple", "arise")
    val solver = ListSolver(smallWordSet, smallWordSet, baseConfig)
    val game = for
      g1 <- Game(5, 6, Judge("suxby", false, smallWordSet))
      g2 <- g1.addGuess("apple")
      g3 <- g2.addGuess("arise")
    yield g3
    solver.solve(game.toOption.get) match
      case Right(_) => fail("should return an error but did not")
      case Left(error) => assert(error == SolverError.GameOver)
  }

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

  it should "make one of two right guesses when there are two" in {
    val game = for
      g1 <- Game(5, 6, Judge("spite", false, wordSet1))
      g2 <- g1.addGuess("apple")
    yield g2
    listSolver.solve(game.toOption.get) match
      case Right(solution) =>
        assert(solution.word == "spite" || solution.word == "spine")
        assert(solution.oneOf == 2)
      case Left(error) => fail("Solve error: " + error)
  }

  it should "choose a word that eliminates the most options" in {
    // correct guess is "forge" because it combines the "f" in place 0 and the "g" in place 3
    val wordSet2 = List("arise", "route", "borne", "force", "forge", "gorge", "horde")
    val game = for
      g1 <- Game(5, 6, Judge("horde", false, wordSet2))
      g2 <- g1.addGuess("arise")
      g3 <- g2.addGuess("route")
    yield g3
    listSolver.solve(game.toOption.get) match
      case Right(solution) =>
        assert(solution.word == "forge")
        assert(solution.oneOf == 5)
      case Left(error) => fail("Solve error: " + error)
  }

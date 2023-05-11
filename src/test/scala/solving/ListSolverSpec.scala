package com.edgiese.wordle1
package solving

import com.edgiese.wordle1.solving.ScorerType.FirstIThoughtOf
import org.scalatest.flatspec.AnyFlatSpec

class ListSolverSpec extends AnyFlatSpec:
  behavior of "a list solver"

  private val wordSet1 = List("apple", "arise", "spine", "spite", "mover", "topaz")
  private val baseConfig = ListSolverConfig(true, FirstIThoughtOf)
  private val listSolver = ListSolver(wordSet1, wordSet1, baseConfig)

  it should "return an error when the game is over" in {
    for
      g1   <- Game("apple", 6, wordSet1, None, false)
      game <- g1.addGuess("apple")
    do listSolver.solve(game) match
      case Right(_) => fail("should return an error but did not")
      case Left(error) => assert(error == SolverError.GameOver)
  }

  it should "resign when there are no more words to try" in {
    val smallWordSet = List("apple", "arise")
    val solver = ListSolver(smallWordSet, smallWordSet, baseConfig)
    for
      g1   <- Game("suxby", 6, smallWordSet, None, false)
      g2   <- g1.addGuess("apple")
      game <- g2.addGuess("arise")
    do solver.solve(game) match
      case Right(_) => fail("should return an error but did not")
      case Left(error) => assert(error == SolverError.GameOver)
  }

  it should "make the right guess when only one works" in {
    for
      g1   <- Game("apple", 6, wordSet1, None, false)
      game <- g1.addGuess("topaz")
    do listSolver.solve(game) match
      case Right(solution) =>
        assert(solution.word == "apple")
        assert(solution.oneOf == 1)
      case Left(error) => fail("Solve error: " + error)
  }

  it should "make one of two right guesses when there are two" in {
    for
      g1   <- Game("spite", 6, wordSet1, None, false)
      game <- g1.addGuess("apple")
    do listSolver.solve(game) match
      case Right(solution) =>
        assert(List("spite", "spine") contains solution.word)
        assert(solution.oneOf == 2)
      case Left(error) => fail("Solve error: " + error)
  }

  /*
  it should "choose a word that eliminates the most options" in {
    // correct guess is "forge" because it combines the "f" in place 0 and the "g" in place 3
    val wordSet2 = List("arise", "route", "borne", "force", "forge", "gorge", "horde")
    val solver = ListSolver(wordSet2, wordSet2, baseConfig)
    for
      g1   <- Game("horde", 6, wordSet2, None, false)
      g2   <- g1.addGuess("arise")
      game <- g2.addGuess("route")
    do solver.solve(game) match
      case Right(solution) =>
        assert(solution.word == "forge")
        assert(solution.oneOf == 5)
      case Left(error) => fail("Solve error: " + error)
  }
  */

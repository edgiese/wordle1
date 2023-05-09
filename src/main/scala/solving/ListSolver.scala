package com.edgiese.wordle1
package solving

case class ListSolverConfig(hardMode: Boolean)

case class ListSolver(answers: List[String], guessWords: List[String], config: ListSolverConfig) extends Solver:
  def solve(game: Game): Either[SolverError, Solution] =
    val regexes = game.getGoodGuesses.fold(List.fill(game.wordLength)(""))
    Left(SolverError.IncompatibleSpec)
  

package com.edgiese.wordle1
package solving

case class ListSolverConfig(hardMode: Boolean)

class ListSolver(answers: List[String], guessWords: List[String], config: ListSolverConfig) extends Solver:
  def solve(game: Game): Either[SolverError, Solution] =
    Left(SolverError.IncompatibleSpec)
  

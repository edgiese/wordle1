package com.edgiese.wordle1
package solving

enum SolverError:
  case IncompatibleSpec, GameOver, Resign

case class Solution(word: String, oneOf: Int, par: Int)

trait Solver {
  def solve(game: Game): Either[SolverError, Solution]
}

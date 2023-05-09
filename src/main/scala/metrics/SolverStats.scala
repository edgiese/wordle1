package com.edgiese.wordle1
package metrics

import solving.Solver

class SolverStats(
                   solver: Solver,
                   maxGuessCount: Int,
                   possibleAnswers: List[String],
                   allowedGuesses: Option[List[String]],
                   hardMode: Boolean):
  ???
package com.edgiese.wordle1
package solving.scorers

import solving.{Scorer, SolverError}

class FirstIThoughtOf extends Scorer:
  override def orderLowToHigh(): Boolean = true

  override def calculateScores(game: Game, guessStrings: List[String]): Either[SolverError, List[(String, Int)]] =
    Right(guessStrings.zipWithIndex)


package com.edgiese.wordle1
package solving.scorers

import solving.{Scorer, SolverError}

class LastFound extends Scorer:
  override def orderLowToHigh(): Boolean = false

  override def calculateScores(game: Game, guessStrings: List[String]): Either[SolverError, List[(String, Int)]] =
    Right(guessStrings.zipWithIndex)


package com.edgiese.wordle1
package solving.scorers

import solving.{Scorer, SolverError}

class LetterCounter extends Scorer:
  override def orderLowToHigh(): Boolean = false

  override def calculateScores(game: Game, guessStrings: List[String]): Either[SolverError, List[(String, Int)]] =
    val letterCounts = guessStrings
      .flatMap(_.toList)
      .foldLeft(Map.empty[Char, Int]){
        (counts, char) => counts + (char -> (counts.getOrElse(char, 0) + 1))
      }
    Right(guessStrings.map(s => (s, s.distinct.foldLeft(0)((sum, c) => sum + letterCounts.getOrElse(c, 0)))))


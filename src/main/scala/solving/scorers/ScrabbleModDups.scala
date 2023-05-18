package com.edgiese.wordle1
package solving.scorers

import solving.{Scorer, SolverError}

class ScrabbleModDups extends Scorer:
  override def orderLowToHigh(): Boolean = true
   /*
   1 point - A, E, I, O, U, L, N, S, T, R.
   2 points - D, G.
   3 points - B, C, M, P.
   4 points - F, H, V, W, Y.
   5 points - K.
   8 points - J, X.
   10 points - Q, Z.
   */
  private val scrabbleValues = Map(
    'a' -> 1, 'b' -> 4, 'c' -> 4, 'd' -> 3, 'e' -> 1, 'f' -> 5, 'g' -> 3, 'h' -> 5, 'i' -> 1, 'j' -> 7, 'k' -> 6,
    'l' -> 2, 'm' -> 4, 'n' -> 2, 'o' -> 1, 'p' -> 4, 'q' -> 7, 'r' -> 2, 's' -> 2, 't' -> 2, 'u' -> 1, 'v' -> 5,
    'w' -> 5, 'x' -> 6, 'y' -> 2, 'z' -> 7
  )

  override def calculateScores(game: Game, guessStrings: List[String]): Either[SolverError, List[(String, Int)]] =
    Right(guessStrings.map(s => (s, s.foldLeft(0)((sum, c) => sum + s.count(c1 => c1 == c) * scrabbleValues.getOrElse(c, 0)))))


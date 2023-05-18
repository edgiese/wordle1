package com.edgiese.wordle1
package solving.scorers

import solving.{Scorer, SolverError}

class ScrabbleDups extends Scorer:
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
    'a' -> 1, 'b' -> 3, 'c' -> 3, 'd' -> 2, 'e' -> 1, 'f' -> 4, 'g' -> 2, 'h' -> 4, 'i' -> 1, 'j' -> 8, 'k' -> 5,
    'l' -> 1, 'm' -> 3, 'n' -> 1, 'o' -> 1, 'p' -> 3, 'q' -> 10,'r' -> 1, 's' -> 1, 't' -> 1, 'u' -> 1, 'v' -> 4,
    'w' -> 4, 'x' -> 8, 'y' -> 4, 'z' -> 10
  )

  override def calculateScores(game: Game, guessStrings: List[String]): Either[SolverError, List[(String, Int)]] =
    Right(guessStrings.map(s => (s, s.foldLeft(0)((sum, c) => sum + s.count(c1 => c1 == c) * scrabbleValues.getOrElse(c, 0)))))


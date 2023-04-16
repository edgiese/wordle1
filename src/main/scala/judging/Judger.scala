package com.edgiese.wordle1
package judging

// A Judger takes a string and returns either guess data or an error indication
trait Judger:
  def judgeGuess(guess: String): Either[BadGuess, Guess]


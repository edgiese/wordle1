package com.edgiese.wordle1

import judging.Judger

// this judger always says every character is incorrect. So everyone wins the game with this one
class AlwaysLose extends Judger:
  def judgeGuess(guessText: String, guesses: List[Either[BadGuess, Guess]]): Either[BadGuess, Guess] =
    Right(Guess(guessText, guessText.map(_ => LetterResult.Unused).toList))


package com.edgiese.wordle1

enum LetterResult:
  case Unused, Exists, Correct

enum GuessError:
  case WrongLength, BadCharacter, NotAWord, HardModeViolation

case class BadGuess(word: String, error: GuessError)

case class Guess(word: String, result: List[LetterResult]):
  def isWinningGuess: Boolean = result.count(_ == LetterResult.Correct) == word.length


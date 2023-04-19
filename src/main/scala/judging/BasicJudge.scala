package com.edgiese.wordle1

package judging

// the BasicJudge trait is a Judger that uses obvious techniques to pre-check a guess for
// bad characters and word length, as well as hard Mode violations. It then calls a method
// with a "predigested" guess that is converted to lowercase.
trait BasicJudge(wordLength: Int, hardMode: Boolean) extends Judger {
  def buildGuess(guess: String): Either[BadGuess, Guess]
  def judgeGuess(guessString: String, guesses: List[Either[BadGuess, Guess]]): Either[BadGuess, Guess] =
    // check for word length
    if guessString.length != wordLength then
      Left(BadGuess(guessString, GuessError.WrongLength))
    else if "[^A-Za-z]".r.matches(guessString) then
      Left(BadGuess(guessString, GuessError.BadCharacter))
    else if hardMode && guesses.exists {
      case Right(Guess(oldGuessString, letterResults)) => letterResults.zipWithIndex.exists {
        case (LetterResult.Unused, _) => false
        case (LetterResult.Correct, ix) => oldGuessString[ix] != guessString[ix]
        // This does not cover complex cases with multiples of letters ... make this better
        case (LetterResult.Exists, ix) => guessString.contains(oldGuessString[ix])
      }
      default => false
    } then
      Left(BadGuess(guessString, GuessError.HardModeViolation))
    else
      buildGuess(guessString.toLowerCase)
}

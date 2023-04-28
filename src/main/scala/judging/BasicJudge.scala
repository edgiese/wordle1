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
    // check for bad characters -- only letters allowed  
    else if "[^A-Za-z]".r.unanchored.matches(guessString) then
      Left(BadGuess(guessString, GuessError.BadCharacter))
    // check for "hard mode" - guesses must conform to previous guess's clues
    // following expression is TRUE if there is a violation... does any guess exist that's a violation?
    else if hardMode && guesses.exists {
      // we ignore error guesses -- Right result only. For Right, look for a faulty character:
      case Right(Guess(oldGuessString, letterResults)) => letterResults.zipWithIndex.exists {
        // if a letter is marked unused, it must not be used in a subsequent guess
        case (LetterResult.Unused, ix) => guessString.contains(oldGuessString.charAt(ix))
        // if a letter is marked "correct" it must be the same in the same place
        case (LetterResult.Correct, ix) => oldGuessString.charAt(ix) != guessString.charAt(ix)
        // if a letter "exists" it must be somewhere in the new guess
        // This does not cover complex cases with multiples of letters ... make this better
        case (LetterResult.Exists, ix) => !guessString.contains(oldGuessString.charAt(ix))
      }
      // ignore Left (bad guesses) in hard mode search
      case _ => false
    } then
      Left(BadGuess(guessString, GuessError.HardModeViolation))
    // all basic checks pass -- convert to lower case and build guess  
    else
      buildGuess(guessString.toLowerCase)
}

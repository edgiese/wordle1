package com.edgiese.wordle1
package judging

import scala.annotation.tailrec

// Implements basic wordle rules to judge a guess.
class Judge(answer: String, hardMode: Boolean, allowedGuesses: List[String]) extends Judger:
  @tailrec
  private def buildLetterResults(guessLets: List[Char], ansLets: List[Char], accumResults: List[LetterResult]): List[LetterResult] =
    if guessLets.isEmpty then
      accumResults
    else if guessLets.head == '_' then
      // if this matches, mark it. We did this ahead of time so that 'Exists' characters report at a lower priority
      buildLetterResults(guessLets.tail, ansLets, accumResults :+ LetterResult.Correct)
    else if ansLets contains guessLets.head then
      // if answer contains guessed letter, not only report that, but remove that letter from the answers
      buildLetterResults(guessLets.tail, ansLets diff Seq(guessLets.head), accumResults :+ LetterResult.Exists)
    else
      buildLetterResults(guessLets.tail, ansLets, accumResults :+ LetterResult.Unused)

  def judgeGuess(guessString: String, guesses: List[Either[BadGuess, Guess]]): Either[BadGuess, Guess] =
    // check for word length
    if guessString.length != answer.length then
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
    else if !allowedGuesses.contains(guessString) then
      Left(BadGuess(guessString, GuessError.NotAWord))
    // all basic checks pass -- convert to lower case and build guess
    else
      // first step is to mark all matching letters.
      val compares = guessString.zip(answer).map((g, a) => if g == a then ('_', '_') else (g, a))
      // now go through and convert guess string to guess result
      Right(Guess(guessString, buildLetterResults(compares.map(_._1).toList, compares.map(_._2).toList, List())))

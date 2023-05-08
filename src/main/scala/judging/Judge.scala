package com.edgiese.wordle1
package judging

import scala.annotation.tailrec

// Implements basic wordle rules to judge a guess.
class Judge(answer: String, hardMode: Boolean, allowedGuesses: List[String]) extends Judger:
  @tailrec
  private def buildResults(gLets: List[Char], aLets: List[Char], acc: List[LetterResult]): List[LetterResult] =
    gLets.headOption match
      // all done
      case None => acc
      // pre-matched "correct"
      case _ if gLets.head == '_'  =>
        buildResults(gLets.tail, aLets, acc :+ LetterResult.Correct)
      // exists -- remove matching char from answer, it's used now
      case _ if aLets contains gLets.head =>
        buildResults(gLets.tail, aLets diff Seq(gLets.head), acc :+ LetterResult.Exists)
      // unused
      case _ =>
        buildResults(gLets.tail, aLets, acc :+ LetterResult.Unused)

  def judgeGuess(guessStringMixed: String, guesses: List[Either[BadGuess, Guess]]): Either[BadGuess, Guess] =
      val guessString = guessStringMixed.toLowerCase()
      // check for word length
      if guessString.length != answer.length then
        Left(BadGuess(guessString, GuessError.WrongLength))
      // check for bad characters -- only letters allowed
      else if "[^a-z]".r.unanchored.matches(guessString) then
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
      else if !allowedGuesses.contains(guessString.toLowerCase) then
        Left(BadGuess(guessString, GuessError.NotAWord))
      // all basic checks pass -- build guess
      else
        // pre-match exact matches. They become _ in guess and are removed from answer
        val compares = guessString.zip(answer).map((g, a) => if g == a then ('_', '_') else (g, a))
        val guessLetters = compares.map(_._1).toList
        val answerLetters = compares.map(_._2).filter(_ != '_').toList
        // convert guess and answer lists to letter results and form Guess
        Right(Guess(guessString, buildResults(guessLetters, answerLetters, List())))

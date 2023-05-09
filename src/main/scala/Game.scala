package com.edgiese.wordle1

import scala.annotation.tailrec

enum LetterResult(annotation: Char):
  case Unused extends LetterResult('_')
  case Exists extends LetterResult('-')
  case Correct extends LetterResult('*')
  def toChar: Char = annotation

enum GuessError:
  case WrongLength, BadCharacter, NotAWord, HardModeViolation

case class BadGuess(word: String, error: GuessError):
  override def toString: String = '"' + word + "\": " + error

case class Guess(word: String, result: List[LetterResult]):
  def isWinningGuess: Boolean = result.count(_ == LetterResult.Correct) == word.length
  override def toString: String = word.zip(result).foldLeft("")((str, ztuple) => str + ztuple._2.toChar + ztuple._1)
enum GameError:
  case BadWordLength, BadTurnCount, BadAnswers, GameOver

enum GameState:
  case Ongoing, Won, Lost

class Game private (
                     val answer: String,
                     val maxGuessCount: Int,
                     val possibleAnswers: List[String],
                     val allowedGuesses: List[String],
                     val guesses: List[Either[BadGuess, Guess]],
                     val hardMode: Boolean,
                     val state: GameState
                   ):
  @tailrec
  private def gameState(foundSoFar: Int, lookIn: List[Either[BadGuess, Guess]]): GameState = lookIn.headOption match
    case None =>
      GameState.Ongoing
    case Some(Right(guess)) if guess.isWinningGuess =>
      GameState.Won
    case Some(Right(_)) if foundSoFar + 1 >= maxGuessCount =>
      GameState.Lost
    case Some(Left(_)) =>
      gameState(foundSoFar, lookIn.tail)
    case Some(Right(_)) =>
      gameState(foundSoFar + 1, lookIn.tail)

  @tailrec
  private def buildResults(gLets: List[Char], aLets: List[Char], acc: List[LetterResult]): List[LetterResult] =
    gLets.headOption match
      // all done
      case None => acc
      // pre-matched "correct"
      case _ if gLets.head == '_' =>
        buildResults(gLets.tail, aLets, acc :+ LetterResult.Correct)
      // exists -- remove matching char from answer, it's used now
      case _ if aLets contains gLets.head =>
        buildResults(gLets.tail, aLets diff Seq(gLets.head), acc :+ LetterResult.Exists)
      // unused
      case _ =>
        buildResults(gLets.tail, aLets, acc :+ LetterResult.Unused)

  private def judgeGuess(guessStringMixed: String, guesses: List[Either[BadGuess, Guess]]): Either[BadGuess, Guess] =
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

  def addGuess(guessString: String): Either[GameError, Game] =
      if (isOver)
        Left(GameError.GameOver)
      else
        val nextGuess = judgeGuess(guessString.toLowerCase, guesses)
        val expandedGuesses = guesses :+ nextGuess
        val nextState = gameState(0, expandedGuesses)
        Right(new Game(answer, maxGuessCount, possibleAnswers, allowedGuesses, expandedGuesses, hardMode, nextState))

  def getAllGuesses: List[Either[BadGuess, Guess]] = guesses
  def getGoodGuesses: List[Guess] = guesses.filter(_.isRight).map(_.toOption.get)

  def isWon: Boolean = state == GameState.Won
  def isLost: Boolean = state == GameState.Lost
  def isOver:  Boolean = state != GameState.Ongoing
  def wordLength: Int = answer.length

  override def toString: String = {
    val guessList = guesses.map({ 
      case Right(g) => g
      case Left(e) => e  
    }).mkString("GUESSES: \n  ", "\n  ", "\n  " + "=" * answer.length * 2)
    val endString = 
      if isLost then
        "game lost. anwer: " + answer 
      else if isWon then 
        "Won game" 
      else 
        "guesses left: " + (maxGuessCount - getGoodGuesses.length)
    "GAME START:\n  hard mode = " + hardMode + "\n  " + guessList + "\n  " + endString + "\nGAME END"
  }


object Game:
  def apply(
             answer: String,
             maxGuessCount: Int,
             possibleAnswers: List[String],
             allowedGuesses: Option[List[String]],
             hardMode: Boolean): Either[GameError, Game] =
    if (answer.isBlank)
      return Left(GameError.BadWordLength)
    if possibleAnswers.exists { _.length != answer.length } then
      return Left(GameError.BadWordLength)
    if allowedGuesses.exists { _.length != answer.length } then
      return Left(GameError.BadWordLength)
    if possibleAnswers.isEmpty then
      return Left(GameError.BadAnswers)
    if !possibleAnswers.contains(answer) then
      return Left(GameError.BadAnswers)
    if 1 > maxGuessCount then
      return Left(GameError.BadTurnCount)
    Right(new Game(answer, maxGuessCount, possibleAnswers, allowedGuesses.getOrElse(possibleAnswers), List.empty, hardMode, GameState.Ongoing))

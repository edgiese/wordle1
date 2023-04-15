package com.edgiese.wordle1

import scala.annotation.tailrec

enum LetterResult:
  case Unused, Exists, Correct

enum GuessError:
  case WrongLength, NotAWord

enum GameError:
  case BadWordLength, BadTurnCount, InconsistentGuess, GameOver

case class BadGuess(word: String, error: GuessError)

case class Guess(word: String, result: List[LetterResult]):
  def isWinningGuess: Boolean = result.count(_ == LetterResult.Correct) == word.length

type Discriminator = String => Either[BadGuess, Guess]

class Game private (
                     wordLength: Int,
                     guessCount: Int,
                     guesses: List[Either[BadGuess, Guess]],
                     discriminator: Discriminator):
  @tailrec
  private def lastGuess(foundSoFar: Int, lookIn: List[Either[BadGuess, Guess]]): Option[Guess] =
    if (lookIn.isEmpty)
      None  // GAME IN PROGRESS
    else
      val guess = lookIn.head
      if (guess.exists(_.isWinningGuess))
        guess.toOption  // WON GAME
      else
        val found = foundSoFar + (if (guess.isRight) 1 else 0)
        if (found >= guessCount)
          guess.toOption   // LOST GAME
        else
          lastGuess(found, lookIn.tail)  // KEEP LOOKING


  def addGuess(guess: String): Either[GameError, Game] =
    if (lastGuess(0, guesses).isDefined)
      Left(GameError.GameOver)
    else
      val nextGuess = discriminator(guess)
      Right(new Game(wordLength, guessCount, guesses :+ nextGuess, discriminator))
      
  def mostRecentGuess(): Option[Guess] = lastGuess(0, guesses)
  
  def getGuesses: List[Either[BadGuess, Guess]] = guesses
  
  def isWon: Boolean = lastGuess(0, guesses).exists(_.isWinningGuess)
  def isLost: Boolean = lastGuess(0, guesses).exists(!_.isWinningGuess)
  def isOver:  Boolean = lastGuess(0, guesses).isDefined
  

object Game:
  def apply(wordLength: Int, guessCount: Int, discriminator: Discriminator): Either[GameError, Game] =
    if (1 > wordLength)
      return Left(GameError.BadWordLength)
    if (1 > guessCount)
      return Left(GameError.BadTurnCount)
    Right(new Game(wordLength, guessCount, List.empty, discriminator))

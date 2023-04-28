package com.edgiese.wordle1

import judging.Judger

import scala.annotation.tailrec

enum GameError:
  case BadWordLength, BadTurnCount, InconsistentGuess, GameOver

class Game private (
                     wordLength: Int,
                     guessCount: Int,
                     guesses: List[Either[BadGuess, Guess]],
                     judger: Judger):
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


  def addGuess(guessString: String): Either[GameError, Game] =
    if (lastGuess(0, guesses).isDefined)
      Left(GameError.GameOver)
    else
      val nextGuess = judger.judgeGuess(guessString, guesses)
      Right(new Game(wordLength, guessCount, guesses :+ nextGuess, judger))
      
  def mostRecentGuess(): Option[Guess] = lastGuess(0, guesses)
  
  def getGuesses: List[Either[BadGuess, Guess]] = guesses
  
  def isWon: Boolean = lastGuess(0, guesses).exists(_.isWinningGuess)
  def isLost: Boolean = lastGuess(0, guesses).exists(!_.isWinningGuess)
  def isOver:  Boolean = lastGuess(0, guesses).isDefined
  

object Game:
  def apply(wordLength: Int, guessCount: Int, judger: Judger): Either[GameError, Game] =
    if (1 > wordLength)
      return Left(GameError.BadWordLength)
    if (1 > guessCount)
      return Left(GameError.BadTurnCount)
    Right(new Game(wordLength, guessCount, List.empty, judger))
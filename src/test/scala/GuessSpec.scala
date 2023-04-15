package com.edgiese.wordle1

import org.scalatest.flatspec.AnyFlatSpec

class GuessSpec extends AnyFlatSpec {
  // this discriminator always says every character is correct. So everyone wins the game with this one
  def alwaysWin(guessText: String): Either[BadGuess, Guess] =
    Right(Guess(guessText, guessText.map(_ => LetterResult.Correct).toList))

  // this discriminator always says every character is unused. So everyone wins the game with this one
  def alwaysLose(guessText: String): Either[BadGuess, Guess] =
    Right(Guess(guessText, guessText.map(_ => LetterResult.Unused).toList))

  behavior of "a game"

  it should "fail to create if letter count is less than 1" in {
    Game(0, 100, alwaysWin) match
      case Left(err) => assert(err == GameError.BadWordLength)
      case Right(_) => fail("This is a bad game request, it should fail")
  }

  it should "fail to create if guess count is less than 1" in {
    Game(6, 0, alwaysWin) match
      case Left(err) => assert(err == GameError.BadTurnCount)
      case Right(_) => fail("This is a bad game request, it should fail")
  }

  it should "have no guesses to begin with" in {
    Game(5, 6, alwaysWin) match
      case Left(_) => fail("Should have been able to create the game")
      case Right(game) => assert(game.getGuesses.isEmpty)
  }

  it should "begin neither won, lost, nor over" in {
    Game(5, 6, alwaysWin) match
      case Left(_) => fail("Should have been able to create the game")
      case Right(game) =>
        assert(!game.isOver)
        assert(!game.isWon)
        assert(!game.isLost)
  }

  it should "correctly mark a win" in {
    Game(5, 6, alwaysWin) match
      case Left(_) => fail("Should have been able to create the game")
      case Right(game) => game.addGuess("arise") match
          case Left(_) => fail("should be able to add first guess")
          case Right(game2) => assert(game2.isWon)
  }


}

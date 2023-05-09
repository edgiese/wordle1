package com.edgiese.wordle1

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Right
import LetterResult._

class GameSpec extends AnyFlatSpec {
  behavior of "a game"

  it should "fail to create if letter count is less than 1" in {
    Game("", 6, List("arise", "mount"), None, false) match
      case Left(err) => assert(err == GameError.BadWordLength)
      case Right(_) => fail("This is a bad game request, it should fail")
  }

  it should "fail to create if guess count is less than 1" in {
    Game("arise", 0, List("arise", "mount"), None, false) match
      case Left(err) => assert(err == GameError.BadTurnCount)
      case Right(_) => fail("This is a bad game request, it should fail")
  }

  it should "fail to create if there are no answers" in {
    Game("arise", 6, List(), None, false) match
      case Left(err) => assert(err == GameError.BadAnswers)
      case Right(_) => fail("This is a bad game request, it should fail")
  }

  it should "fail to create if answer is not in list" in {
    Game("arise", 6, List("mount"), None, false) match
      case Left(err) => assert(err == GameError.BadAnswers)
      case Right(_) => fail("This is a bad game request, it should fail")
  }

  it should "have no guesses to begin with" in {
    Game("arise", 6, List("arise","mount"), None, false) match
      case Left(_) => fail("Should have been able to create the game")
      case Right(game) =>
        assert(game.guesses.isEmpty)
        assert(!game.isOver)
        assert(!game.isWon)
        assert(!game.isLost)
  }

  it should "correctly mark a win" in {
    val gameOrError = for
      g0 <- Game("arise", 6, List("arise","mount"), None, false)
      g1 <- g0.addGuess("arise")
    yield g1
    gameOrError match
      case Left(error) => fail("Game or guess error: " + error)
      case Right(game) =>
        assert(game.isOver)
        assert(game.isWon)
  }

  it should "correctly mark a loss" in {
    val gameOrError = for
      g0 <- Game("arise", 2, List("arise", "mount", "catty"), None, false)
      g1 <- g0.addGuess("mount")
      g2 <- g1.addGuess("catty")
    yield g2
    gameOrError match
      case Left(error) => fail("Game error: " + error)
      case Right(game) =>
        assert(game.isOver)
        assert(game.isLost)
  }

  it should "convert guesses to lowercase" in {
    val gameOrError = for
      g0 <- Game("arise", 2, List("arise", "mount", "catty"), None, false)
      g1 <- g0.addGuess("ARiSE")
    yield g1
    gameOrError match
      case Left(error) => fail("Game or guess error: " + error)
      case Right(game) =>
        assert(game.isOver)
        assert(game.isWon)
  }

  it should "return a bad guess if length is wrong" in {
    val gameOrError = for
      g0 <- Game("catty", 2, List("arise", "mount", "catty"), None, false)
      g1 <- g0.addGuess("cat")
    yield g1
    gameOrError match
      case Left(error) => fail("Game error: " + error)
      case Right(game) => game.guesses.headOption match
        case None => fail("must return a guess")
        case Some(guess) => guess match
          case Left(badGuess: BadGuess) => assert(badGuess == BadGuess("cat", GuessError.WrongLength))
          case Right(_) => fail("should be an error guess")
  }

  it should "return a bad guess if non-letters present" in {
    val gameOrError = for
      g0 <- Game("catty", 2, List("arise", "mount", "catty"), None, false)
      g1 <- g0.addGuess("cats!")
    yield g1
    gameOrError match
      case Left(error) => fail("Game error: " + error)
      case Right(game) => game.guesses.headOption match
        case None => fail("must return a guess")
        case Some(guess) => guess match
          case Left(badGuess: BadGuess) => assert(badGuess == BadGuess("cats!", GuessError.BadCharacter))
          case Right(_) => fail("should be an error guess")
  }

  // the following game is used for several hard mode tests
  private val hardModeTestGame = (for
    g0   <- Game("smite", 6, List("arise", "spine", "spike", "apple", "spite", "knife", "smite"), None, true)
    g1   <- g0.addGuess("aris")  // should be 5 chars, ignore this bad guess
    g2   <- g1.addGuess("arise") // a_r_i*s-e*
    g3   <- g2.addGuess("apise") // not a word
    g4   <- g3.addGuess("spine") // s*p_i*n_e*
  yield g4).toOption.get

  it should "return a hard-mode violation for repeated incorrect characters" in {
    val gameOrError = hardModeTestGame.addGuess("arise")
    gameOrError match
      case Left(error) => fail("Game error: " + error)
      case Right(game) => game.guesses.reverse.headOption match
        case None => fail("must return a guess")
        case Some(guess) => guess match
          case Left(badGuess: BadGuess) => assert(badGuess == BadGuess("arise", GuessError.HardModeViolation))
          case Right(_) => fail("should be an error guess")
  }

  it should "return a hard-mode violation for missing correct characters" in {
    println(hardModeTestGame)
    val gameOrError = hardModeTestGame.addGuess("smoke")
    gameOrError match
      case Left(error) => fail("Game error: " + error)
      case Right(game) => game.guesses.reverse.headOption match
        case None => fail("must return a guess")
        case Some(guess) => guess match
          case Left(badGuess: BadGuess) => assert(badGuess == BadGuess("smoke", GuessError.HardModeViolation))
          case Right(_) => fail("should be an error guess")
  }

  it should "return a hard-mode violation for missing existing characters" in {
    val gameOrError = hardModeTestGame.addGuess("knife")
    gameOrError match
      case Left(error) => fail("Game error: " + error)
      case Right(game) => game.guesses.reverse.headOption match
        case None => fail("must return a guess")
        case Some(guess) => guess match
          case Left(badGuess: BadGuess) => assert(badGuess == BadGuess("knife", GuessError.HardModeViolation))
          case Right(_) => fail("should be an error guess")
  }

  it should "pass on a guess that meets hard-mode criteria" in {
    val gameOrError = hardModeTestGame.addGuess("smite")
    gameOrError match
      case Left(error) => fail("Game error: " + error)
      case Right(game) => assert(game.isWon)
  }

  it should "properly handle duplicated characters" in {
    val gameOrError = for
      g0 <- Game("wheee", 6, List("wheee", "eeemk"), None, false)
      g1 <- g0.addGuess("eeemk")
    yield g1
    gameOrError match
      case Left(error) => fail("Game error: " + error)
      case Right(game) => game.guesses.headOption match
        case None => fail("must return a guess")
        case Some(guess) => guess match
          case Left(_) => fail("guess should be good")
          case Right(guess) => assert(guess.toString == "e-e-e*m_k_")
  }

  it should "duplicate guesses from NYT" in {
    val gameOrError = for
      g0 <- Game("circa", 6, List("arise", "rapid", "cigar", "circa"), None, false)
      g1 <- g0.addGuess("arise")
      g2 <- g1.addGuess("rapid")
      g3 <- g2.addGuess("cigar")
    yield g3
    val listOfGuesses = List(Right(Guess("arise", List(Exists, Exists, Exists, Unused, Unused))),
                             Right(Guess("rapid", List(Exists, Exists, Unused, Exists, Unused))),
                             Right(Guess("cigar", List(Correct, Correct, Unused, Exists, Exists))))
    gameOrError match
      case Left(error) => fail("Game error: " + error)
      case Right(game) => assert(game.guesses == listOfGuesses)

  }
}

package com.edgiese.wordle1
package judging

import org.scalatest.flatspec.AnyFlatSpec

private class GenericBasicJudge(hardMode: Boolean, build: String => Either[BadGuess, Guess]) extends BasicJudge(5, hardMode):
  def buildGuess(guess: String): Either[BadGuess, Guess] = build(guess)


class BasicJudgeSpec extends AnyFlatSpec:
  behavior of "a generic basic judge"

  it should "convert guesses to lowercase" in {
    val gbj = GenericBasicJudge(true, guess => {
      assert(guess == "catty")
      Left(BadGuess("ignore", GuessError.BadCharacter))
    })
    gbj.judgeGuess("Catty", Nil)
  }

  it should "return a bad guess if length is wrong" in {
    val gbj = GenericBasicJudge(true, guess => {
      fail("Should have failed before calling buildGuess")
      Left(BadGuess("ignore", GuessError.BadCharacter))
    })
    assert(gbj.judgeGuess("cat", Nil) == Left(BadGuess("cat", GuessError.WrongLength)))
  }

  it should "return a bad guess if non-letters present" in {
    val gbj = GenericBasicJudge(true, guess => {
      fail("Should have failed before calling buildGuess")
      Left(BadGuess("ignore", GuessError.BadCharacter))
    })
    assert(gbj.judgeGuess("cats!", Nil) == Left(BadGuess("cats!", GuessError.BadCharacter)))
  }

  import LetterResult._
  // fixture of game history used for several "hard mode" tests below
  // correct answer is "smite"
  private val guessHistory = List(
    Left(BadGuess("aris", GuessError.WrongLength)),   // should be 5 chars... ignore this guess for hard mode
    Right(Guess("arise", List(Unused, Unused, Correct, Exists, Correct))),
    Left(BadGuess("apise", GuessError.NotAWord)),
    Right(Guess("spine", List(Correct, Unused, Correct, Unused, Correct))),
  )

  it should "accept a guess in non-hard mode that does not conform to old guesses" in {
    val gbj = GenericBasicJudge(false, guess => {
      assert(guess == "apple")
      Left(BadGuess("ignored", GuessError.BadCharacter))
    })
    gbj.judgeGuess("apple", guessHistory)
  }

  it should "return a hard-mode violation for repeated incorrect characters" in {
    val gbj = GenericBasicJudge(true, guess => {
      fail("Should have failed before calling buildGuess")
      Left(BadGuess("ignore", GuessError.BadCharacter))
    })
    assert(gbj.judgeGuess("arise", guessHistory) == Left(BadGuess("arise", GuessError.HardModeViolation)))
  }

  it should "return a hard-mode violation for missing correct characters" in {
    val gbj = GenericBasicJudge(true, guess => {
      fail("Should have failed before calling buildGuess")
      Left(BadGuess("ignore", GuessError.BadCharacter))
    })
    assert(gbj.judgeGuess("smoke", guessHistory) == Left(BadGuess("smoke", GuessError.HardModeViolation)))
  }

  it should "return a hard-mode violation for missing existing characters" in {
    val gbj = GenericBasicJudge(true, guess => {
      fail("Should have failed before calling buildGuess")
      Left(BadGuess("ignore", GuessError.BadCharacter))
    })
    assert(gbj.judgeGuess("knife", guessHistory) == Left(BadGuess("knife", GuessError.HardModeViolation)))
  }

  it should "pass on a guess that meets hard-mode criteria" in {
    val winningGuess = Guess("smite", List(Correct, Correct, Correct, Correct, Correct))
    val gbj = GenericBasicJudge(true, _ => {
      Right(winningGuess)
    })
    assert(gbj.judgeGuess("smite", guessHistory) == Right(winningGuess))
  }

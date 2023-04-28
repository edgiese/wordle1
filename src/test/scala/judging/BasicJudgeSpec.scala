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
    assert(gbj.judgeGuess("cat!", Nil) == Left(BadGuess("cat!", GuessError.WrongLength)))
  }

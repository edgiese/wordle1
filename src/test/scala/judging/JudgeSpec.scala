package com.edgiese.wordle1
package judging

import org.scalatest.flatspec.AnyFlatSpec
import LetterResult.*

import scala.util.Right

class JudgeSpec extends AnyFlatSpec:
  behavior of "a judge"

  private val allCorrect: List[LetterResult] = List(Correct, Correct, Correct, Correct, Correct)

  it should "convert guesses to lowercase" in {
    val gbj = Judge("catty", true, List("catty", "doggy"))
    assert(gbj.judgeGuess("Catty", Nil) == Right(Guess("catty", allCorrect)))
  }

  it should "return a bad guess if length is wrong" in {
    val gbj = Judge("catty", true, List("catty", "doggy"))
    assert(gbj.judgeGuess("cat", Nil) == Left(BadGuess("cat", GuessError.WrongLength)))
  }

  it should "return a bad guess if non-letters present" in {
    val gbj = Judge("catty", true, List("catty", "doggy"))
    assert(gbj.judgeGuess("cats!", Nil) == Left(BadGuess("cats!", GuessError.BadCharacter)))
  }

  // fixture of game history used for several "hard mode" tests below
  // correct answer is "smite"
  private val guessHistory = List(
    Left(BadGuess("aris", GuessError.WrongLength)),   // should be 5 chars... ignore this guess for hard mode
    Right(Guess("arise", List(Unused, Unused, Correct, Exists, Correct))),
    Left(BadGuess("apise", GuessError.NotAWord)),
    Right(Guess("spine", List(Correct, Unused, Correct, Unused, Correct))),
  )
  private val allowedWords = List("arise", "spine", "spike", "apple", "spite", "knife", "smite")

  it should "accept a guess in non-hard mode that does not conform to old guesses" in {
    val gbj = Judge("smite", false, allowedWords)
    assert (gbj.judgeGuess("apple", guessHistory) == Right(Guess("apple", List(Unused, Unused, Unused, Unused, Correct))))
  }

  it should "return a hard-mode violation for repeated incorrect characters" in {
    val gbj = Judge("smite", true, allowedWords)
    assert(gbj.judgeGuess("arise", guessHistory) == Left(BadGuess("arise", GuessError.HardModeViolation)))
  }

  it should "return a hard-mode violation for missing correct characters" in {
    val gbj = Judge("smite", true, allowedWords)
    assert(gbj.judgeGuess("smoke", guessHistory) == Left(BadGuess("smoke", GuessError.HardModeViolation)))
  }

  it should "return a hard-mode violation for missing existing characters" in {
    val gbj = Judge("smite", true, allowedWords)
    assert(gbj.judgeGuess("knife", guessHistory) == Left(BadGuess("knife", GuessError.HardModeViolation)))
  }

  it should "pass on a guess that meets hard-mode criteria" in {
    val gbj = Judge("smite", true, allowedWords)
    assert(gbj.judgeGuess("smite", guessHistory) == Right(Guess("smite", allCorrect)))
  }

  it should "properly handle duplicated characters" in {
    val gbj = Judge("wheee", true, List("wheee", "eeemk"))
    assert(gbj.judgeGuess("eeemk", Nil) == Right(Guess("eeemk", List(Exists, Exists, Correct, Unused, Unused))))
  }

  it should "handle first guess copied from NYT" in {
    val gbj = Judge("circa", false, List("arise", "rapid", "cigar", "circa"))
    assert(gbj.judgeGuess("arise", Nil) == Right(Guess("arise", List(Exists, Exists, Exists, Unused, Unused))))
  }

  it should "handle second guess copied from NYT" in {
    val gbj = Judge("circa", false, List("arise", "rapid", "cigar", "circa"))
    assert(gbj.judgeGuess("rapid", Nil) == Right(Guess("rapid", List(Exists, Exists, Unused, Exists, Unused))))
  }

  it should "handle third guess copied from NYT" in {
    val gbj = Judge("circa", false, List("arise", "rapid", "cigar", "circa"))
    assert(gbj.judgeGuess("cigar", Nil) == Right(Guess("cigar", List(Correct, Correct, Unused, Exists, Exists))))
  }

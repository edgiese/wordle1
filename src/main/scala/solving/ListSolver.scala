package com.edgiese.wordle1
package solving

import solving.ListSolver.filterAnswers
import solving.scorers._

import com.edgiese.wordle1.solving.ScorerType.FirstIThoughtOf

// Scorer trait used to tie together the different scoring methods. The one used for the solver is
// specified in the config
trait Scorer:
  def calculateScores(game: Game, guessStrings: List[String]): Either[SolverError, List[(String, Int)]]
  def orderLowToHigh(): Boolean

enum ScorerType:
  case FirstIThoughtOf

case class ListSolverConfig(hardMode: Boolean, scorerType: ScorerType, defaultFirst: String = "arise")

case class ListSolver(answers: List[String], guessWords: List[String], config: ListSolverConfig) extends Solver:
  private final val scorer = config.scorerType match
    case FirstIThoughtOf => new FirstIThoughtOf()

  private def sorter(a:(String, Int), b:(String, Int)): Boolean = 
    if scorer.orderLowToHigh() then
      a._2 > b._2
    else
      a._2 < b._2  

  def solve(game: Game): Either[SolverError, Solution] =
    if game.isOver then
      return Left(SolverError.GameOver)
    if game.guesses.isEmpty then
      return Right(Solution(config.defaultFirst, 4500, 1))
    // for each position in answer, make a list of letters/results for all the good guesses
    val matchingAnswers = filterAnswers(answers, game.getGoodGuesses)
    if matchingAnswers.isEmpty then
      // none of the possible answers match
      return Left(SolverError.Resign)
    if matchingAnswers.tail.isEmpty then
      // only one answer -- return it
      return Right(Solution(matchingAnswers.head, 1, 1))

    // return first best-scored answer or error if it occurred
    scorer.calculateScores(game, matchingAnswers) match
      case Left(error) => Left(error)
      case Right(guessesAndScores) =>
        val chosen = guessesAndScores.sortWith(sorter).head
        Right(Solution(chosen._1, matchingAnswers.length, chosen._2))


object ListSolver:
  def filterAnswers(answers: List[String], guesses: List[Guess]): List[String] =
    if guesses.isEmpty then
      return answers
    // guesses are a list of letter results collected by guess. combine characters with results and transpose so that
    // we have a list collected by letter, in order. so n Guesses becomes 5 Letter-result lists 
    // (5 or however many characters long the answer is)
    val letterGuesses: List[List[(Char, LetterResult)]] = guesses
      .map(guess => guess.word.toList.zip(guess.result))
      .transpose

    // list of letters excluded by position
    val excludedByPos = letterGuesses.map(_.filter(_._2 == LetterResult.Exists).map(_._1).distinct)
    val excludedInWord = excludedByPos.flatten.toSet
    // all unused characters across all position -- because of doubled letters, we're going to pass on excluding
    // letters that occur somewhere, but are in the wrong position
    val unused = (letterGuesses.flatten.filter(_._2 == LetterResult.Unused).map(_._1).toSet -- excludedInWord).toList
    // all "used somewhere" characters across all positions
    val used = letterGuesses.flatten.filter(_._2 == LetterResult.Exists).map(_._1).distinct
    // list of required letters by position
    val required = letterGuesses.map(lg => lg.find(_._2 == LetterResult.Correct))

    // build a regex string that will match if a word is line with guesses up until now -- everything except "exists"
    val exclRegexString = required.zip(excludedByPos).foldLeft("")((str, zippedReqExc) =>
      val req = zippedReqExc._1
      val exc = zippedReqExc._2
      val regexStringForCharPosition = if req.nonEmpty then
        req.get._1.toString
      else if exc.isEmpty && unused.isEmpty then
        "."
      else
        "[^" + (exc ::: unused).mkString + "]"
      str + regexStringForCharPosition
    )
    val regexes = used.map(_.toString.r.unanchored) :+ exclRegexString.r.unanchored
    answers.filter(answer => regexes.forall(_.matches(answer)))

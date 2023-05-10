package com.edgiese.wordle1
package solving

import solving.ListSolver.filterAnswers

case class ListSolverConfig(hardMode: Boolean, defaultFirst: String = "arise")

case class ListSolver(answers: List[String], guessWords: List[String], config: ListSolverConfig) extends Solver:
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

    // there is more than one solution. For now, only return the first one. add more options to improve later
    val nextGuess = matchingAnswers.head
    Right(Solution(nextGuess, matchingAnswers.length, 1))


object ListSolver:
  def filterAnswers(answers: List[String], guesses: List[Guess]): List[String] =
    if guesses.isEmpty then
      return answers
    // guesses are a list of letter results collected by guess. combine characters with results and transpose so that
    // we have a list collected by letter, in order. so n Guesses becomes 5 Letter-result lists 
    // (or however long the answer is)
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

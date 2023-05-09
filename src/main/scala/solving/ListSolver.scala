package com.edgiese.wordle1
package solving

case class ListSolverConfig(hardMode: Boolean)

case class ListSolver(answers: List[String], guessWords: List[String], config: ListSolverConfig, par: Int = 1) extends Solver:
  def solve(game: Game): Either[SolverError, Solution] =
    if game.isOver then
      return Left(SolverError.GameOver)
    // for each position in answer, make a list of letters/results for all the good guesses
    val letterGuesses: List[List[(Char, LetterResult)]] = game
      .getGoodGuesses
      .map(guess => guess.word.toList.zip(guess.result))
      .transpose

    // all unused characters across all position
    val unused = letterGuesses.flatten.filter(_._2 == LetterResult.Unused).map(_._1).distinct
    // all "used somewhere" characters across all positions
    val used = letterGuesses.flatten.filter(_._2 == LetterResult.Exists).map(_._1).distinct
    // list of letters excluded by position
    val excluded = letterGuesses.map(_.filter(_._2 == LetterResult.Exists).map(_._1).distinct)
    // list of required letters by position
    val required = letterGuesses.map(lg => lg.find(_._2 == LetterResult.Correct))

    // build a regex that will match if a word is line with guesses up until now -- everything except "exists"
    val exclRegexString = required.zip(excluded).foldLeft("")((str, zippedReqExc) =>
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
    val regexes = used.map(_.toString.r.unanchored) :+ exclRegexString.r
    val matchingAnswers = answers.filter(answer => regexes.forall(_.matches(answer)))
    if matchingAnswers.isEmpty then
      // none of the possible answers match
      return Left(SolverError.Resign)
    if matchingAnswers.tail.isEmpty then
      // only one answer -- return it
      return Right(Solution(matchingAnswers.head, 1, par))

    // there is more than one solution. For now, only return the first one. add more options to improve later
    Right(Solution(matchingAnswers.head, matchingAnswers.length, par))


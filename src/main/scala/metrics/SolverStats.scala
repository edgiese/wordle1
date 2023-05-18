package com.edgiese.wordle1
package metrics

import solving.{Solver, SolverError}

import scala.annotation.tailrec

case class SolverCounts(total: Int, won: Int, wonTurns: Int, lost: Int, error: Int)

class SolverStats(
                   solver: Solver,
                   maxGuessCount: Int,
                   possibleAnswers: List[String],
                   allowedGuesses: Option[List[String]],
                   hardMode: Boolean):
  @tailrec
  final def playOneGame(answer: String, maybeGame: Option[Game] = None): Either[SolverError, Game] =
    maybeGame match
      case None => Game(answer, maxGuessCount, possibleAnswers, allowedGuesses, hardMode) match
        case Left(_) => Left(SolverError.IncompatibleSpec)
        case Right(game) => playOneGame(answer, Some(game))
      case Some(game) if game.isOver => Right(game)
      case Some(game) =>
          solver.solve(game) match
            case Left(error) => Left(error)
            case Right(solution) => game.addGuess(solution.word) match
              case Left(_) => Left(SolverError.IncompatibleSpec)
              case Right(nextGame) => playOneGame(answer, Some(nextGame))

  private def rollUpStats(answer: String, counts: SolverCounts): SolverCounts =
    playOneGame(answer) match
      case Left(_) => counts.copy(total=counts.total+1, error=counts.error+1)
      case Right(game) =>
        if game.isWon then
          counts.copy(total=counts.total+1, won=counts.won+1, wonTurns=counts.wonTurns+game.getGoodGuesses.length)
        else
          println(game)
          counts.copy(total=counts.total+1, lost=counts.lost+1)

  def calculateStats(): SolverCounts =
    possibleAnswers.foldLeft(SolverCounts(0, 0, 0, 0, 0))((sc, answer) => rollUpStats(answer, sc))

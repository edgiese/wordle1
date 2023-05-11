package com.edgiese.wordle1
package metrics

import LetterResult.*
import solving.{ListSolver, ListSolverConfig}

import com.edgiese.wordle1.solving.ScorerType.FirstIThoughtOf
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source
import scala.util.Right

class SolverStatsSpec extends AnyFlatSpec:
  behavior of "solver stats generator"

  it should "work in a simple case" in {
    val source = Source.fromFile("C:\\Users\\edward.p.giese\\IdeaProjects\\wordle1\\words5.txt")
    val words5 = source.getLines.toList.map(_.trim)
    source.close()

    val listSolver = ListSolver(words5, words5, ListSolverConfig(false, FirstIThoughtOf, defaultFirst = "grace"))
    val stats = SolverStats(listSolver, 6, words5, None, false)

    val gResult = stats.playOneGame("ethic")
    println(gResult)
    assert(gResult.isRight)
  }
  
  it should "calculate statistics" in {
    val source = Source.fromFile("C:\\Users\\edward.p.giese\\IdeaProjects\\wordle1\\words5.txt")
    val regex = "[^a-z]".r.unanchored
    val words5 = source.getLines.toList.map(_.trim).filter(!regex.matches(_))
    source.close()

    val listSolver = ListSolver(words5, words5, ListSolverConfig(false, FirstIThoughtOf))
    val stats = SolverStats(listSolver, 6, words5, None, false)

    val counts = stats.calculateStats()
    println(counts)
    assert(counts.total > 4000)
  }


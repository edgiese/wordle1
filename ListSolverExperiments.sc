import com.edgiese.wordle1._
import com.edgiese.wordle1.solving.{ListSolver, ListSolverConfig, ScorerType}
import com.edgiese.wordle1.metrics.SolverStats
import scala.io.Source

val source = Source.fromFile("C:\\Users\\edward.p.giese\\IdeaProjects\\wordle1\\words5.txt")
val regex = "[^a-z]".r.unanchored
val words5 = source.getLines.toList.map(_.trim).filter(!regex.matches(_))
source.close()

val listSolver = ListSolver(words5, words5, ListSolverConfig(false, ScorerType.FirstIThoughtOf, defaultFirst = "grace"))
val stats = SolverStats(listSolver, 6, words5, None, false)

val nextGuess = for
  game <- Game("ethic", 6, words5, None, false)
  g1 <- game.addGuess("grace")
  g2 <- game.addGuess("chest")
  solution <- listSolver.solve(g2)
yield solution

val gResult = stats.playOneGame("ethic")
// val counts = stats.calculateStats()

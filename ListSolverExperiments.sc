import com.edgiese.wordle1._
import com.edgiese.wordle1.solving._
import scala.io.Source

val source = Source.fromFile("C:\\Users\\edward.p.giese\\IdeaProjects\\wordle1\\words5.txt")
val words5 = source.getLines.toList.map(_.trim)
source.close()

def pullGame(either: Either[GameError, Game]): Game = either match {
  case Right(game) => game
  case Left(_) => null
}

val g0 = pullGame(Game("cocoa", 6, words5, None, false))
val g1 = pullGame(g0.addGuess("grace"))

val solver = ListSolver(words5, words5, ListSolverConfig(true))

val guess1 = solver.solve(g1)
val g2 = pullGame(g1.addGuess("achoo"))
val guess2 = solver.solve(g2)
val g3 = pullGame(g2.addGuess("bacon"))
val guess3 = solver.solve(g3)
val g4 = pullGame(g3.addGuess("cocoa"))

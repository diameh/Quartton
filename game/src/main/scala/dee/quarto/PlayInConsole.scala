package dee.quarto

import java.util.{InputMismatchException, Scanner}
import scala.io.StdIn._

/**
 * Created by diameh on 22/03/2017.
 */
object PlayInConsole {

  def main(args: Array[String]): Unit = {
    println("To start a game, enter two player names")
    println("First player:")
    val player1 = Player(readLine())
    println("Second player:")
    val player2 = Player(readLine())
    //how long should we wait?
    val game = playItUntilYouCantPlayNoMore(Game(player1, player2))
    printEndScore(game)
  }

  def playItUntilYouCantPlayNoMore(game: Game): Game = {
    printTheStatus(game)
    if (game.board.unusedPos.isEmpty) game
    else if (game.board.unusedFigures.isEmpty) game.makeAMove(tryToGetAPos(game.board.unusedPos))
    else playItUntilYouCantPlayNoMore(game.makeAMove(tryToGetAPos(game.board.unusedPos), Some(tryToGetAFigure(game.board.unusedFigures))))
  }

  def printTheStatus(game: Game): Unit = {
    println(s"Current player: ${game.currentP.name}");
    printScore(game)
    printTheBoard(game)
    if (!game.board.unusedPos.isEmpty) {
      print("All available positions: ")
      for (pos <- game.board.unusedPos) print(s"$pos ")
      println()
    }
    if (!game.board.unusedFigures.isEmpty) {
      print("All available figures: ")
      for (fig <- game.board.unusedFigures) print(s"$fig ")
      println()
    }
    if (game.board.figureToMove.isDefined) println(s"Chosen figure to play: ${game.board.figureToMove.get}");
  }

  def tryToGetAPos(allAvailablePos: Seq[Pos]): Pos = {
    attemptThreeTimes(getAPosition(allAvailablePos), 0)
  }

  def tryToGetAFigure(allAvailableFigures: Set[Figure]): Figure = {
    attemptThreeTimes(getAFigure(allAvailableFigures), 0)
  }

  def attemptThreeTimes[P](getSomething: => P, attempt: Int): P = {
    if (attempt > 3) endGame()
    else {
      try {
        getSomething
      } catch {
        case m: InputMismatchException => attemptThreeTimes(getSomething, attempt + 1)
      }
    }
  }

  def getAPosition(positions: Seq[Pos]): Pos = {
    println("Choose a row and column where to move the given figure, for example position " + positions.head.row + " " + positions.head.col)
    val line = new Scanner(readLine())
    val row = line.nextInt
    val col = line.nextInt
    line.close()
    if (positions.contains(Pos(row, col))) Pos(row, col)
    else {
      throw new InputMismatchException("Could not understand the position")
    }
  }

  def getAFigure(figures: Set[Figure]): Figure = {
    println("Choose a figure for the other player's move, for example " + figures.head.toString())
    val line = readLine()
    if (line.length == 4) {
      figures.find(_.toString == line.toUpperCase) match {
        case None => getAFigure(figures)
        case Some(fig) => fig
      }
    } else {
      throw new InputMismatchException("Could not understand the figure")
    }
  }

  def endGame(): Nothing = {
    println("You seem too tired to play")
    throw new RuntimeException("All tries up")
  }

  def printScore(game: Game): Unit = {
     for ((player, score) <- game.players) println(s"The score of ${player.name} is $score")
  }

  def printEndScore(game: Game): Unit = {
    val winnerScore = game.players.values.max
    if (game.players.values.sum - winnerScore == winnerScore) println("It's a tie! Play again to find out who's smarter?")
    for ((player, score) <- game.players)
      if (score == winnerScore) println(s"${player.name} won! Congrats!")
  }

  def printTheBoard(game: Game): Unit = {
    for (pos <- Board.allPositions) {
      game.board.filledPositions.get(pos) match {
        case None => print("  [    ]  ")
        case Some(fig) => print(s"   $fig   ")
      }
      pos match {
        case Pos(_, 3) => println("")
        case _ =>
      }
    }
  }
}
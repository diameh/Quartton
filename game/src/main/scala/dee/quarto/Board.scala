package dee.quarto

import scala.util.Random

/**
 * Created by diameh on 31/08/15.
 */
object Board {

  //not generated before used first, lazy uses locks because there might be a lot of threads trying to use it for the first time
  //something that should not be used hotpath (locks not good for performance)
  lazy val allPositions = (for {
    x <- (0 to 3)
    y <- (0 to 3)
  } yield Pos(x, y)).toSeq

  // what the compiler unfolds/desugars the for comprehension to
  // lazy val allPositinsExpl =
//    (0 to 3).flatMap { x =>
//      (0 to 3).map (y => Pos(x, y))
//    }

  //referentially transparent = def can be replaced with val because the value of it is never changed, we don't even have input
  val allFigures = for {
    c <- Set(Light, Dark)   //it is enough when outermost is Set then it will yield to a Set
    h <- Seq(Short, Tall)
    s <- Seq(Round, Square)
    f <- Seq(Filled, Hollow)
  } yield Figure(c, h, s, f)

  def randomFigure = allFigures.toSeq(Random.nextInt(allFigures.size - 1))

  def apply(): Board = {
    val randomFigure = Board.randomFigure
    new Board(Some(randomFigure), Board.allFigures - randomFigure, Board.allPositions, Map())
  }
}

class Board(
             val figureToMove: Option[Figure],
             val unusedFigures: Set[Figure],
             val unusedPos: Seq[Pos],
             val filledPositions: Map[Pos, Figure]
             )
{
  require(figureToMove.isEmpty && filledPositions.size == 16 || !unusedFigures.contains(figureToMove.get),
    "Chosen figure must not be in the set of unused figures" + figureToMove.get + " " + filledPositions.size)

  def makeMove(pos: Pos, nextFigure: Option[Figure]): MoveResult = {
    nextFigure match {
      case None =>  if (filledPositions.size != 15) FailedMove("It is not end of game, choose a figure")
                    else completeASuccessfulMove(pos, nextFigure, unusedFigures)
      case Some(figure) =>  if (!unusedFigures.contains(figure)) FailedMove("Figure already used")
                            else if (!unusedPos.contains(pos)) FailedMove("Position already used")
                            else completeASuccessfulMove(pos, nextFigure, unusedFigures - nextFigure.get)
    }
  }

  private def completeASuccessfulMove(pos: Pos, nextFigure: Option[Figure], newUnusedFigures: Set[Figure]): SuccessfulMove = {
    //we make a move with the previous chosenForMove
    val newFilledPositions = filledPositions + (pos -> figureToMove.get)
    SuccessfulMove(new Board(nextFigure, newUnusedFigures, unusedPos.filterNot(e => e == pos), newFilledPositions), calculateScore(newFilledPositions, pos))
  }

  def calculateScore(filledPositions: Map[Pos, Figure], lastAdded: Pos) = {
    score(getRow(filledPositions, lastAdded).toSet) + score(getColumn(filledPositions, lastAdded).toSet)
  }

  def getRow(filledPositions: Map[Pos, Figure], lastAdded: Pos) = {
    filledPositions.collect {
      case (Pos(lastAdded.row, _), figure) => figure
    }
  }

  def getColumn(filledPositions: Map[Pos, Figure], lastAdded: Pos) = {
    filledPositions.collect {
      case (Pos(_, lastAdded.col), figure) => figure
    }
  }

  def score(figures: Set[Figure]): Int = {
    if (figures.size == 4) 8 - getUniqueFeatures(figures).size
    else 0
  }

  def getUniqueFeatures(row: Set[Figure])= {
    for {
      fig <- row
      feature <- Set(fig.c, fig.h, fig.s, fig.f)
    } yield feature
  }
}

sealed trait MoveResult //sealed = subtyped only in this file
case class SuccessfulMove(board: Board, score: Int) extends MoveResult
case class FailedMove(message: String) extends MoveResult
//do we need a gameover


case class Pos(row: Int, col: Int) {
  override def toString(): String = "("+ row + " " + col + ")"
}

case class Figure(c: FigColor, h: Height, s: FigShape, f: Fill) {
  override def toString(): String = c.toString + h.toString + s.toString + f.toString
}

//hint toString should return the name, could be made into a trait and reused
trait ToStringIsName {
   def name: String
   override def toString = name
}

sealed abstract class FigColor(val name: String) extends ToStringIsName
case object Light extends FigColor("L")
case object Dark extends FigColor("D")

sealed abstract class Height(val name: String) extends ToStringIsName
case object Short extends Height("S")
case object Tall extends Height("T")

sealed abstract class FigShape(val name: String) extends ToStringIsName
case object Round extends FigShape("R")
case object Square extends FigShape("Q")

sealed abstract class Fill(val name: String) extends ToStringIsName
case object Filled extends Fill("F")
case object Hollow extends Fill("H")





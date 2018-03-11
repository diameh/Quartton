package dee.quarto

import akka.actor.{Actor, ActorRef, Props, Terminated}

//SUPER
//request for a link to a game - superviser?
//send a link to a game with ID - superviser?
//creates a game with ref to 2 players





//PLAYER ACTOR
//player receives message to play or to watch, or game over
//send messages "move"
//send quit early



object GameActor {

  case class WatchGame(board: Board, score: Map[ActorRef, Int])
  case class PlayGame(board: Board, score: Map[ActorRef, Int])
  case class PerformMove(pos: Pos, nextFigure: Option[Figure])
  case class MoveFailed(reason: String)
  case class GameOver(why: String, board: Board, score: Map[ActorRef, Int])
  case object Quit

  def props(player1: ActorRef, player2: ActorRef) = Props(new GameActor(player1, player2))

}

class GameActor(player1: ActorRef, player2: ActorRef) extends Actor {

  import GameActor._

  var board: Board = createBoard
  var currentPlayer: ActorRef = player1
  var scoreTable = Map(player1 -> 0, player2 -> 0)
  context.watch(player1)
  context.watch(player2)
  player1 ! PlayGame(board, scoreTable)
  player2 ! WatchGame(board, scoreTable)

  def createBoard = {
    Board()
  }

  override def receive: Receive = {
    case PerformMove(pos, nextFigure) if sender() == currentPlayer => applyMoveAndNotify(pos, nextFigure)
    case PerformMove(pos, nextFigure) => sender() ! MoveFailed("Not your turn")
    case Quit => endTheGame("quit was chosen")
    case Terminated(who) => endTheGame("terminated")
  }


  private def endTheGame(why: String) = {
    //figure out who won + scores
    player1 ! GameOver(s"$why ", board, scoreTable)
    player2 ! GameOver(s"$why ", board, scoreTable)
    context.stop(self)
  }

  private def applyMoveAndNotify(pos: Pos, nextFigure: Option[Figure]) = {
    board.makeMove(pos, nextFigure) match {
      case SuccessfulMove(newBoard, score) =>
        scoreTable = scoreTable.updated(currentPlayer, scoreTable(currentPlayer) + score)
        board = newBoard
        if (newBoard.unusedPos.isEmpty) endTheGame("")
        else switchTurnsAndNotify()
      case FailedMove(message) => sender() ! MoveFailed(message)
    }
  }

  def switchTurnsAndNotify() = {
    currentPlayer ! WatchGame(board, scoreTable)
    currentPlayer = if (currentPlayer == player1) player2 else player1
    currentPlayer ! PlayGame(board, scoreTable)
  }

}


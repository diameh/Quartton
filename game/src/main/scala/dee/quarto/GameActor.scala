package dee.quarto

import akka.actor.{ActorRef, Actor}

object Game {
  def apply(player1: Player, player2: Player): Game = {
    new Game(Map(player1 -> 0, player2 -> 0), player1, Board())
  }
}
// has to be immutable
case class Game(players: Map[Player, Int], currentP: Player, board: Board) {
  def makeAMove(position: Pos): Game = {
    makeAMove(position, None)
  }

  def makeAMove(position: Pos, figure: Option[Figure]): Game = {
    board.makeMove(position, figure) match {
      case SuccessfulMove(newBoard, score) =>
        val newPlayers = players + (currentP -> (players(currentP) + score))
        val newCurrentP = (players.keySet - currentP).head
        new Game(newPlayers, newCurrentP, newBoard)
      case FailedMove(message) => //write tests for wrong figure, pos, missing fig chosen
        throw new RuntimeException(s"Move failed $message")
    }
  }
}

object GameActor {

  case object GetState
  case class GetStateReply(state: Game)
  case class PerformMove(someKindOfDescription: Any)

}

class GameActor(player1: ActorRef, player2: ActorRef) extends Actor {

  import GameActor._

  //var state: Game = Game()
  //create board
  //make a move, receive Either[WhyItWasIllegal, Board]

  var currentPlayer: ActorRef = player1


  override def receive: Receive = {

    //case GetState => sender() ! GetStateReply(game)
    case _ =>
  }
}

case class Player(name: String)


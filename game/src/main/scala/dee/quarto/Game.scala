package dee.quarto

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

case class Player(name: String)


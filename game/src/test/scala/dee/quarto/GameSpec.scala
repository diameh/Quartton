package dee.quarto

import org.scalatest._


class GameSpec extends WordSpec with Matchers {


  "A Game" should {

    "start out with a valid initial state" in {
      val game = Game(Player("first"), Player("second"))
      game.board.filledPositions.isEmpty should be (true)
      game.players.keySet.size should be (2)
      game.players.keySet.contains(game.currentP) should be (true)
    }

    "play some moves" in {
      //outrageous things bf has claimed
      //val player: Option[Player] = Some(null)  --doesn't make much sense, combo of bad design and Java compatibility
      //Option(null) == None

      val firstFigure: Figure = new Figure(Light, Tall, Square, Hollow)
      val player1 = Player("Wiener")
      val player2 = Player("Null")
      val game = new Game(Map(player1 -> 0, player2 -> 0), player1, new Board(Some(firstFigure), Board.allFigures - firstFigure, Board.allPositions, Map()))

      val gameWithNoScore = game.makeAMove(Pos(0,0), Some(new Figure(Dark, Tall, Square, Hollow))) //player1
        .makeAMove(Pos(0,1), Some(new Figure(Light, Short, Square, Filled)))
        .makeAMove(Pos(0,2), Some(new Figure(Light, Short, Round, Filled))) //player1
        .makeAMove(Pos(0,3), Some(new Figure(Dark, Tall, Square, Filled)))
      gameWithNoScore.players.get(player1) should be (0)
      gameWithNoScore.players.get(player2) should be (0)


      val gameWithScoreOneAdded = gameWithNoScore.makeAMove(Pos(1,0), Some(new Figure(Dark, Short, Square, Filled))) //player1
        .makeAMove(Pos(3,0), Some(new Figure(Dark, Short, Square, Hollow)))
        .makeAMove(Pos(2,0), Some(new Figure(Light, Tall, Round, Hollow))) //player1
      gameWithScoreOneAdded.players.get(player1) should be (1)
      gameWithScoreOneAdded.players.get(player2) should be (0)


      val gameWithScoreTwoAdded = gameWithScoreOneAdded.makeAMove(Pos(2,2), Some(new Figure(Light, Short, Round, Hollow)))
        .makeAMove(Pos(2,3), Some(new Figure(Dark, Tall, Round, Hollow))) //player1
        .makeAMove(Pos(1,1), Some(new Figure(Dark, Tall, Round, Filled)))
        .makeAMove(Pos(3,1), Some(new Figure(Dark, Short, Round, Hollow))) //player1
        .makeAMove(Pos(2,1), Some(new Figure(Light, Short, Square, Hollow)))
      gameWithScoreTwoAdded.players.get(player1) should be (1)
      gameWithScoreTwoAdded.players.get(player2) should be (2)

      val gameEndsWithScoreOneAdded = gameWithScoreTwoAdded.makeAMove(Pos(3,2), Some(new Figure(Dark, Short, Round, Filled))) //player1
        .makeAMove(Pos(1,2), Some(new Figure(Light, Tall, Square, Filled)))
        .makeAMove(Pos(3,3), Some(new Figure(Light, Tall, Round, Filled))) //player
        .makeAMove(Pos(1,3), None)
      gameEndsWithScoreOneAdded.players.get(player1) should be (1)
      gameEndsWithScoreOneAdded.players.get(player2) should be (3)
    }
  }

}

package dee.quarto

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{TestKit, TestProbe}
import dee.quarto.GameActor.{MoveFailed, PerformMove, Quit}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class GameActorSpec extends TestKit(ActorSystem()) with WordSpecLike with Matchers with BeforeAndAfterAll {

  class TestGameActor(player1: ActorRef, player2: ActorRef) extends GameActor(player1, player2) {
    override def createBoard: Board = {
      val firstFigure: Figure = Figure(Light, Tall, Square, Hollow)
      new Board(Some(firstFigure), Board.allFigures - firstFigure, Board.allPositions, Map())
    }
  }

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "GameActor" should {
    "send play to first player and watch to the other" in {
      val player1 = TestProbe()
      val player2 = TestProbe()
      val gameActor = system.actorOf(GameActor.props(player1.ref, player2.ref))
      val playGameMessage1 = player1.expectMsgType[GameActor.PlayGame]
      all (playGameMessage1.score.values) should === (0)
      val playGameMessage2 = player2.expectMsgType[GameActor.WatchGame]
      all (playGameMessage2.score.values) should === (0)
    }

    "finish game when player wants to quit" in {
      val player1 = TestProbe()
      val player2 = TestProbe()
      val gameActor = system.actorOf(GameActor.props(player1.ref, player2.ref))
      gameActor.tell(Quit, player1.ref)
      player1.expectMsgType[GameActor.PlayGame]
      player2.expectMsgType[GameActor.WatchGame]

      player1.expectMsgType[GameActor.GameOver]
      player2.expectMsgType[GameActor.GameOver]

      player1.watch(gameActor)
      player1.expectTerminated(gameActor)
    }

    "not accept a move from anyone else than current player" in {
      val player1 = TestProbe()
      val player2 = TestProbe()
      val gameActor = system.actorOf(GameActor.props(player1.ref, player2.ref))
      player1.expectMsgType[GameActor.PlayGame]
      player2.expectMsgType[GameActor.WatchGame]

      gameActor.tell(PerformMove(Pos(0,0), None), player2.ref)
      player2.expectMsgType[MoveFailed]
    }

    "play the game" in {
      val player1 = TestProbe()
      val player2 = TestProbe()
      val gameActor = system.actorOf(Props(new TestGameActor(player1.ref, player2.ref)))
      player1.expectMsgType[GameActor.PlayGame]
      player2.expectMsgType[GameActor.WatchGame]

      def sendFinalMove(sender: TestProbe, position: Pos) = {
        gameActor.tell(PerformMove(position, None), sender.ref)
        val otherPlayer = if (sender == player1) player2 else player1
        sender.expectMsgType[GameActor.GameOver]
        val result = otherPlayer.expectMsgType[GameActor.GameOver]
        (result.board, result.score)
      }

      def sendAMove(sender: TestProbe, position: Pos, figure: Figure) = {
        gameActor.tell(PerformMove(position, Some(figure)), sender.ref)
        val otherPlayer = if (sender == player1) player2 else player1
        sender.expectMsgType[GameActor.WatchGame]
        val result = otherPlayer.expectMsgType[GameActor.PlayGame]
        (result.board, result.score)
      }

      sendAMove(player1, Pos(0,0), Figure(Dark, Tall, Square, Hollow))
      sendAMove(player2, Pos(0,1), Figure(Light, Short, Square, Filled))

      sendAMove(player1, Pos(0,2), Figure(Light, Short, Round, Filled))
      val (currBoard1, currScore1) = sendAMove(player2, Pos(0,3), Figure(Dark, Tall, Square, Filled))
      currScore1(player1.ref) should === (0)
      currScore1(player2.ref) should === (0)

      sendAMove(player1, Pos(1,0), Figure(Dark, Short, Square, Filled))
      sendAMove(player2, Pos(3,0), Figure(Dark, Short, Square, Hollow))

      val (currBoard2, currScore2) = sendAMove(player1, Pos(2,0), Figure(Light, Tall, Round, Hollow))
      currScore2(player1.ref) should === (1)
      currScore2(player2.ref) should === (0)
      sendAMove(player2, Pos(2,2), Figure(Light, Short, Round, Hollow))

      sendAMove(player1, Pos(2,3), Figure(Dark, Tall, Round, Hollow))
      sendAMove(player2, Pos(1,1), Figure(Dark, Tall, Round, Filled))

      sendAMove(player1, Pos(3,1), Figure(Dark, Short, Round, Hollow))
      val (currBoard3, currScore3) = sendAMove(player2, Pos(2,1), Figure(Light, Short, Square, Hollow))
      currScore3(player1.ref) should === (1)
      currScore3(player2.ref) should === (2)

      sendAMove(player1, Pos(3,2), Figure(Dark, Short, Round, Filled))
      sendAMove(player2, Pos(1,2), Figure(Light, Tall, Square, Filled))

      sendAMove(player1, Pos(3,3), Figure(Light, Tall, Round, Filled))
      val (currBoard4, currScore4) = sendFinalMove(player2, Pos(1,3))
      currScore4(player1.ref) should === (1)
      currScore4(player2.ref) should === (3)
    }
  }
}

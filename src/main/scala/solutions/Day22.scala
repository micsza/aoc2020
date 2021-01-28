package solutions

import solutions.Day22.domain.{Deck, GameConfig}

import scala.annotation.tailrec

object Day22 {


  object domain {

    type Card = Int

    case class Deck(cards: Vector[Card]) {

      def isEmpty: Boolean = this.cards.isEmpty

      def size: Int = this.cards.length

      def pop: (Card, Deck) = (this.cards.head, Deck(this.cards.drop(1)))

      def enq(c: Card): Deck =  Deck(this.cards.appended(c))

      def first(n: Int): Deck = Deck(this.cards.take(n))

      def score: Int = {
        val n = this.cards.length
        val v = this.cards.zipWithIndex.map(p => (p._1, n - p._2))
        v.foldLeft(0) { case (acc, (c, j)) => acc + c * j}
      }

      def round(that: Deck): (Deck, Deck) = {
        if (this.isEmpty || that.isEmpty)
          (this, that)
        else {
          val (thisCard, thisDeque) = this.pop
          val (thatCard, thatDeque) = that.pop

//          println(s"\t\t player 1 plays $thisCard, player 2 plays: $thatCard.")

          if (thisCard > thatCard) (thisDeque.enq(thisCard).enq(thatCard), thatDeque)
          else ( thisDeque, thatDeque.enq(thatCard).enq(thisCard))
        }

      }
    }

    trait Winner
    case object Player1Wins extends Winner
    case object Player2Wins extends Winner

    case class GameConfig(player1: Deck, player2: Deck)

    case class Game(id: Long, level: Long, gameConfig: GameConfig, history: Set[GameConfig]) {

      def prettyPrint: Unit = {
        println(s"-- Game Config level = $level, id = $id--")
        println(s"\tplayer 1: ${gameConfig.player1.cards}")
        println(s"\tplayer 2: ${gameConfig.player2.cards}")
      }

      def recursiveCombat: (Winner, Int) = {
//        Thread.sleep(200)

        prettyPrint
        if (gameConfig.player1.isEmpty) {
//          println(s"*** [$id] Player 1 empty => Player 2 wins")
          (Player2Wins, gameConfig.player2.score)
        }
        else {
          if (gameConfig.player2.isEmpty || (history contains gameConfig)) {
            if (history contains(gameConfig))
              println(s"---- [level = $level, id = $id] HISTORY (size = ${history.size}) REPEATS!!!!")
//            println(s"*** [$id] Player 2 empty or history repeats => Player 1 wins")
            (Player1Wins, gameConfig.player1.score)
          }
          else {
            val (player1Card, player1Rest) = gameConfig.player1.pop
            val (player2Card, player2Rest) = gameConfig.player2.pop

            if (player1Rest.size >= player1Card && player2Rest.size >= player2Card) {
              // sub game
//              println(s"[$id] will play a subgame with id = ${id * 10}...")
              val (subResult, _) = Game(id * 10, level + 1, GameConfig(player1Rest.first(player1Card), player2Rest.first(player2Card)), Set.empty).recursiveCombat
//              println(s"[$id] BACK from subgame with result $subResult")
              subResult match {
                case Player1Wins =>
//                  println(s"[$id] player 1 wins ...")
                  val pl1 = player1Rest.enq(player1Card).enq(player2Card)
                  Game(id + 1, level, GameConfig(pl1, player2Rest), history + this.gameConfig).recursiveCombat
                case Player2Wins =>
                  val pl2 = player2Rest.enq(player2Card).enq(player1Card)
//                  println(s"[$id] player 1 wins ...")
                  Game(id + 1, level, GameConfig(player1Rest, pl2), history + this.gameConfig).recursiveCombat
              }
            }
            else {
              val (pl1, pl2) = gameConfig.player1.round(gameConfig.player2)
              Game(id + 1, level, GameConfig(pl1, pl2), history + gameConfig).recursiveCombat
            }

          }

        }

      }
    }
  }

  object part1 {
    import domain._

    def solution(deque1: Deck, deque2: Deck): Card = {

      @tailrec
      def go(deq1: Deck, deq2: Deck): (Deck, Deck) = {
        if (deq1.isEmpty || deq2.isEmpty) (deq1, deq2)
        else {
          val (d1, d2) = deq1.round(deq2)
          go(d1, d2)
        }
      }

      val (d1, d2) = go(deque1, deque2)
      if (d1.isEmpty) d2.score
      else d1.score
    }
  }

  object part2 {
    import domain._

    def solution(deck1: Deck, deck2: Deck): (Winner, Int) = {
      val game = Game(0L, 0L, GameConfig(deck1, deck2), Set.empty)
      game.recursiveCombat
    }

    type Cards = Vector[Card]
    type GameState = (Cards, Cards)
    type GameHistory = Set[GameState]

    def simpleGame(gameState: GameState, history: GameHistory): GameState = {
      if (gameState._1.isEmpty || gameState._2.isEmpty) gameState
      else {
        if (history contains gameState) (gameState._1, Vector.empty)
        else {
          val player1Card = gameState._1.head
          val player1Tail = gameState._1.tail
          val player2Card = gameState._2.head
          val player2Tail = gameState._2.tail
          val state =
            if (player1Tail.length >= player1Card && player2Tail.length >= player2Card) {
              // subgame
              val newGameState = (player1Tail.take(player1Card), player2Tail.take(player2Card))
              val subGameResult = simpleGame(newGameState, Set.empty)
              if (subGameResult._2.isEmpty) {
                (player1Tail.appended(player1Card).appended(player2Card), player2Tail)

              }
              else {
                (player1Tail, player2Tail.appended(player2Card).appended(player1Card))
              }
            }
            else {
              // no subgame
              gameState
            }

          // main game



        }
      }

    }
  }


}

object Day22App extends App {
  import Day22._
  import domain._

  val test1 = Deck(Vector(9, 2, 6, 3, 1))
  val test2 = Deck(Vector(5, 8, 4, 7, 10))

  val player1 = Vector(29, 30, 44, 35, 27, 2, 4, 38, 45, 33, 50, 21, 17, 11, 25, 40, 5, 43, 41, 24, 12, 19, 23, 8, 42)
  val player2 = Vector(32, 13, 22, 7, 31, 16, 37, 6, 10, 20, 47, 46, 34, 39, 1, 26, 49, 9, 48, 36, 14, 15, 3, 18, 28)

//  val test1Sol = part1.solution(test1, test2)
//  println(test1Sol)
//
  val sol1 = part1.solution(Deck(player1), Deck(player2))
  println(s"Solution for part 1 is $sol1.")

  /*
  part 2
   */

//  val cs1 = Vector(9, 2, 6, 3, 1)
//  val cs2 = Vector(5, 8, 4, 7, 10)
//  val gc = Game(0, GameConfig(Deck(cs1), Deck(cs2)), Set.empty)
//  val (test2winner, test2score) = gc.recursiveCombat
//  println(s"Test Solution for part 2: winner is $test2winner, score is $test2score.")

  val (winner, score) = part2.solution(Deck(player1), Deck(player2))
  println(s"Solution for part 2 is: winner is $winner, score is $score.")

}

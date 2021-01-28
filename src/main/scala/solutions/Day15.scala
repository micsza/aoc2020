package solutions

object Day15 {

  object domain {
    type Turn = Int
    type Number = Long
  }

  object part1 {
    import domain._

    def solution(longs: List[Number], targetTurn: Turn): Number = {

      def go(turn: Turn, prevNum: Number, ageMap: Map[Number, Turn]): Number = {
        val currentNum = if (ageMap.contains(prevNum)) (turn - 1) - ageMap(prevNum) else 0
        //println(s"[$turn] map = $ageMap\n\t current = $currentNum")
        val updatedAgeMap = ageMap.updated(prevNum, turn - 1)
        if (turn == targetTurn) currentNum
        else go(turn + 1, currentNum, updatedAgeMap)
      }

      val initMap: Map[Number, Turn] = longs.zipWithIndex.map{case (n, t) => (n, t + 1)}.toMap
      println(initMap)
      go(initMap.size + 1, longs.last, initMap)
    }
  }


}

object Day15App extends App {

  import Day15._

  val testInput = List(0, 3, 6).map(_.toLong)
  val test2Input = List(3L, 1L, 2L)
  val dataInput = List(15, 5, 1, 4, 7, 0).map(_.toLong)

  val sol1 = part1.solution(dataInput, 30000000)
  println(s"Solution for part1 is $sol1.")
}

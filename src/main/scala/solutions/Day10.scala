package solutions

import utils.utils.readElemsNewLine

object Day10 {
  object utils {

  }

  object part1 {

    def solution(as: List[Int]): Long = {
      val bs = as.sorted.drop(1) ::: List(as.max + 3)
      val cs = as.zip(bs).map(p => p._2 - p._1)
      val oneJoltDiff = cs.count(_ == 1)
      val threeJoltDiff = cs.count(_ == 3)
      oneJoltDiff.toLong * threeJoltDiff
    }

  }

  object part2 {
    def bar(n: Int): Int = n match {
      case 2 => 2
      case 3 => 4
      case 4 => 7
      case _ => 1
    }

  }

}

object Day10App extends App {
  import Day10._
  //Solution for part 1 is 2470.

  val dataPath = "src/main/resources/input/day10.in"
  val testPath = "src/main/resources/input/day10.test"

  val as = readElemsNewLine(testPath).map(_.toInt).toList.sorted
  val bs = readElemsNewLine(dataPath).map(_.toInt).toList.sorted

  println(as)
  println(bs)

  val sol1 = part1.solution(0 :: bs)
  println(s"Solution for part 1 is $sol1.")

  val oneBlocks = List(
    3,
    4,
    3,
    3,
    1,
    1,
    3,
    2,
    3,
    4,
    3,
    2,
    3,
    2,
    4,
    4,
    3,
    3,
    1,
    4,
    2,
    4,
    3
  )

  val res = oneBlocks map part2.bar
  val longs = res map(_.toLong)
  println(longs.product)
}

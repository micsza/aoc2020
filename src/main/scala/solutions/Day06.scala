package solutions

object Day06 {

  object input {
    import utils.utils._
    def readInputForPart1(filePath: String): List[Set[Char]] = {
      val ss = readEmptyNewLineSeparatedStrings(filePath)
      ss.map(_.trim.filter(_ > ' ').toSet)
    }

    def readInputForPart2(filePath: String): List[String] = {
      val ss = readEmptyNewLineSeparatedStrings(filePath)
      ss
      //ss.map(_.trim.filter(_ > ' ').toSet)
    }
  }

  object part1 {
    def solution(ls: List[Set[Char]]): Int = {
      ls.foldLeft(0: Int)((acc, s) => acc + s.size)
    }
  }

  object part2 {
    def solution(filePath: String): Unit = {

    }
  }
}

object Day06App extends App {
  import Day06._
  val dataPath = "src/main/resources/input/day06.in"
  val testPath = "src/main/resources/input/day06.test"

  val sets1 = input.readInputForPart1(dataPath)
  val sol1 = part1.solution(sets1)
  println(s"Solution for part 1 is $sol1.")

  val ls = input.readInputForPart2(dataPath)
  val res: List[List[String]] = ls.map(s => s.split("\n").toList)
  for (r <- res) println(s"--\n$r")

  val what = res.map(_.reduce(_ intersect _))
  for (w <- what) println(s"--\n$w")
  val sol2 = what.foldLeft(0: Int)(_ + _.length)
  println(s"Solution for part 2 is $sol2.")




}

package solutions

import utils.utils.readElemsNewLine

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day09 {

  object utils {
    def readInput(filePath: String, n: Int = 25): (List[Long], Queue[Long], List[Long]) = {
      val all = readElemsNewLine(filePath).map(_.toLong).to(List)
      val queue = all.take(n).to(Queue)
      val rest = all.drop(n)

      (all, queue, rest)
    }

    def allPairSums(as: Queue[Long]): Queue[Long] = {
      for {
        a <- as
        b <- as if (a < b)
      } yield a + b
    }
  }

  object part1 {
    def solution(pair: (Queue[Long], List[Long])): Option[Long] = {
      val (preamble, rest) = pair

      def go(preamble: Queue[Long], rest: List[Long]): Option[Long] = rest match {
        case Nil => None
        case r :: rest1 =>
          val sums = utils.allPairSums(preamble)
          if (sums.contains(r)) {
            val newPreamble = preamble.dequeue._2.enqueue(r)
            go(newPreamble, rest1)
          } else Some(r)
      }

      go(preamble, rest)
    }
  }

  object part2 {
    def solution(all: List[Long], target: Long, minLength: Int = 2) = {
      @tailrec
      def go(q: Queue[Long], bs: List[Long]): Option[Queue[Long]] = {
        val sum = q.sum
        if (q.size < minLength || sum < target) {
          bs match {
            case Nil => None
            case b :: bs1 => go(q.enqueue(b), bs1)
          }
        }
        else {
          if (sum == target) {
            Some(q)
          }
          else {
            go(q.dequeue._2, bs)
          }
        }
      }
      val subSeq = go(Queue.empty[Long], all)
      subSeq.map { q => q.min + q.max}
    }
  }
}

object Day09App extends App {
  import Day09._

  val dataPath = "src/main/resources/input/day09.in"
  val testPath = "src/main/resources/input/day09.test"

  // test
  val testInput = utils.readInput(testPath, 5)
  println(s"test input: $testInput")
  val testSol1 = part1.solution((testInput._2, testInput._3))
  println(s"Solution for part 1 is $testSol1.")
  val testSol2 =
    testSol1.flatMap { sol1 =>
      part2.solution(testInput._1, sol1)
    }
  println(s"Solution for part 2 is $testSol2")

  // data
  val input = utils.readInput(dataPath)
  val sol1Opt = part1.solution((input._2, input._3))
  val sol2Opt =
    sol1Opt.flatMap { sol1 => part2.solution(input._1, sol1)}
  println(s"Solution for part 1 is $sol1Opt.\nSolution for part 2 is $sol2Opt.")
}

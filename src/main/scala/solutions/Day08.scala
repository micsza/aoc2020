package solutions


import utils.utils.readElemsNewLine

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day08 {

  object domain {
    trait Op
    case class Nop(arg: Long) extends Op
    case class Acc(arg: Long) extends Op
    case class Jmp(arg: Int) extends Op
  }

  object utils {
    import domain._

    def parseOp(str: String): Op = {
      val ss = str.split(' ')
      ss.head match {
        case "nop" => Nop(ss(1).toLong)
        case "acc" => Acc(ss(1).toLong)
        case "jmp" => Jmp(ss(1).toInt)
        case _ => throw new Exception("unknown op code")
      }
    }

    def readInput(filePath: String): List[Op] = {
      val strings = readElemsNewLine(filePath).toList
      strings map parseOp
    }
  }

  object part1 {
    import domain._

    def solution(ops: List[Op]): Long = {
      @tailrec
      def go(idx: Int, visited: List[Int], acc: Long): Long = {
        if (visited.contains(idx)) {
          acc
        }
        else {
          ops(idx) match {
            case Nop(_) => go(idx + 1, idx :: visited, acc)
            case Acc(arg) => go(idx + 1, idx :: visited, acc + arg)
            case Jmp(arg) => go(idx + arg, idx :: visited, acc)
          }
        }
      }
      go(0, Nil, 0)
    }
  }

  object part2 {
    import domain._

    def cycle(ops: Vector[Op]): (Option[List[Int]], Long) = {
      @tailrec
      def go(idx: Int, visited: List[Int], acc: Long): (Option[List[Int]], Long) = {
        if (idx >= ops.size) {
          (None, acc)
        }
        else {
          if (visited.contains(idx)) {
            (Some(idx :: visited), acc)
          }
          else {
            ops(idx) match {
              case Nop(_) => go(idx + 1, idx :: visited, acc)
              case Acc(arg) => go(idx + 1, idx :: visited, acc + arg)
              case Jmp(arg) => go(idx + arg, idx :: visited, acc)
            }
          }
        }
      }
      go(0, Nil, 0)
    }

    def solution(operations: Vector[Op]): Option[Long] = {
      @tailrec
      def go(ops: Vector[Op], idx: Int): Option[Long] = {
        if (idx >= ops.size) {
          None
        }
        else {
          ops(idx) match {
            case Acc(_) => go(ops, idx + 1)
            case Jmp(arg) =>
              cycle(ops.updated(idx, Nop(arg))) match {
                case (None, acc) => Some(acc)
                case _ => go(ops, idx + 1)
              }
            case Nop(arg) =>
              cycle(ops.updated(idx, Jmp(arg.toInt))) match {
                case (None, acc) => Some(acc)
                case _ => go(ops, idx + 1)
              }
          }
        }
      }

      go(operations, 0)
    }

  }
}

object Day08App extends App {
  import Day08._

  val dataPath = "src/main/resources/input/day08.in"
  val testPath = "src/main/resources/input/day08.test"

  val testOps = utils.readInput(testPath)
  val ops = utils.readInput(dataPath)

  val testSol1 = part1.solution(testOps)
  println(s"Test sol = $testSol1")

  val sol1 = part1.solution(ops)
  println(s"Solution for part 1 is $sol1.")

  val testCycle = part2.cycle(testOps.toVector)
  println(s"Test cycle = $testCycle")

  val sol2 = part2.solution(ops.toVector)
  println(s"Solution for part 2 is $sol2.")




}
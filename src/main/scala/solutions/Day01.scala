package solutions

import utils.utils.readElemsNewLine

import scala.annotation.tailrec

object Part1 {

  def solution(as: List[Int], target: Int): Option[(Int, Int)] = {
    @tailrec
    def inner(a: Int, bs: List[Int]): Option[Int] = bs match {
      case b :: bs1 => if (a + b == target) Some(b) else inner(a, bs1)
      case Nil => None
    }

    @tailrec
    def outer(bs: List[Int]): Option[(Int, Int)] = bs match {
      case b :: bs1 => inner(b, bs1) match {
        case None => outer(bs1)
        case Some(c) => Some(b, c)
      }
      case Nil => None
    }
    outer(as)
  }
}

object Part2 {

  def solution(as: List[Int], target: Int): Option[(Int, Int, Int)] = {
    @tailrec
    def outer(bs: List[Int]): Option[(Int, Int, Int)] = bs match {
      case Nil => None
      case b :: bs1 => Part1.solution(bs1, target - b) match {
        case None => outer(bs1)
        case Some((a, c)) => Some((b, a, c))
      }
    }
    outer(as)
  }
}

object Day01 extends App {
  val dataPath = "src/main/resources/input/day1.in"
  val as = readElemsNewLine(dataPath).map(_.toInt).toList

  Part1.solution(as, 2020) match {
    case Some((a, b)) => println(s"Solution for part 1 is $a * $b = ${a * b}.")
    case None => println("No solution for part 1.")
  }

  Part2.solution(as, 2020) match {
    case Some((a, b, c)) => println(s"Solution for part 2 is $a * $b * $c = ${a * b * c}.")
    case None => println("No solution for part 2.")
  }
}

package solutions

import solutions.Day03.input.readGridPattern
import utils.utils.readElemsNewLine

import scala.annotation.tailrec

object Day03 {
  sealed trait Elem
  case object Tree extends Elem
  case object Open extends Elem

  type Position = (Int, Int)

  case class Grid(pattern: Vector[Vector[Elem]]) {
    val height: Int = pattern.size
    val width: Int = pattern(0).size
    def elem(position: Position): Option[Elem] =
      if (position._1 < height)
        Some(pattern(position._1)(position._2 % width))
      else None
  }

  object Grid {
    def apply(dataPath: String): Grid = Grid(readGridPattern(dataPath))
  }

  object input {
    implicit def charToElem(z: Char): Elem = z match {
      case '.' => Open
      case '#' => Tree
      case _ => throw new IllegalArgumentException("Unknown char.")
    }

    def readGridPattern(dataPath: String): Vector[Vector[Elem]] = {
      val ss = readElemsNewLine(dataPath)
      ss.map(_.toVector.map(charToElem)).toVector
    }
  }

  object part1 {
    def solution(grid: Grid, jumpDown: Int = 1, jumpRight: Int = 3): Int = {
      @tailrec
      def go(position: Position, counter: Int): Int = grid.elem(position) match {
        case None => counter
        case Some(e) => {
          val nextPosition = (position._1 + jumpDown, position._2 + jumpRight)
          e match {
            case Tree => go(nextPosition, counter + 1)
            case Open => go(nextPosition, counter)
          }
        }
      }

      go((0, 0), 0)
    }
  }

  object part2 {
    def solution(grid: Grid): Long = {
      // todo with fold, overflow better approach?
      val count11 = part1.solution(grid, jumpDown = 1, jumpRight = 1)
      val count13 = part1.solution(grid, jumpDown = 1, jumpRight = 3)
      val count15 = part1.solution(grid, jumpDown = 1, jumpRight = 5)
      val count17 = part1.solution(grid, jumpDown = 1, jumpRight = 7)
      val count21 = part1.solution(grid, jumpDown = 2, jumpRight = 1)
      val res: Long = count11.toLong * count13 * count15 * count17 * count21
      res
    }

  }
}

object Day03App extends App {
  import Day03._
  val dataPath = "src/main/resources/input/day3.in"
  val testDataPath = "src/main/resources/input/day3.test"
  val vs = input.readGridPattern(dataPath)
  val grid = Grid(dataPath)

  def sol1 = part1.solution(grid)
  def sol2 = part2.solution(grid)
  println(s"Solution for part1 is $sol1.\nSolution for part 2 is $sol2.")

}

package solutions

import solutions.Day11.domain.Grid
import solutions.Day11.input.readGridPattern
import utils.utils.readElemsNewLine
import scala.language.postfixOps

import scala.annotation.tailrec
import scala.language.implicitConversions

object Day11 {

  object domain {

    sealed trait Elem
    case object Floor extends Elem
    trait Seat extends Elem
    case object Empty extends Seat
    case object Occupied extends Seat

    case class Position(coordinates: (Int, Int)) {
      def row: Int = coordinates._1

      def col: Int = coordinates._2

      def neighbours: List[Position] = {
        val ps =
          for {
            i <- this.row - 1 to this.row + 1
            j <- this.col - 1 to this.col + 1 if ((i, j) != (this.row, this.col))
          } yield (i, j)

        ps.map { p => Position((p._1, p._2))}.toList
      }

      def next(direction: Direction): Position = {
        val (i, j) = direction match {
          case North          => (-1,  0)
          case NorthEast      => (-1, +1)
          case East           => ( 0, +1)
          case SouthEast      => (+1, +1)
          case South          => (+1,  0)
          case SouthWest      => (+1, -1)
          case West           => ( 0, -1)
          case NorthWest      => (-1, -1)
        }
        Position((this.row + i, this.col + j))
      }
    }

    trait Direction
    case object North extends Direction
    case object NorthEast extends Direction
    case object East extends Direction
    case object SouthEast extends Direction
    case object South extends Direction
    case object SouthWest extends Direction
    case object West extends Direction
    case object NorthWest extends Direction

    object Position {
      def start: Position = Position(0, 0)
    }

    case class Grid(pattern: Vector[Vector[Elem]], threshold: Int = 4) {
      val height: Int = pattern.size
      val width: Int = pattern(0).size

      def validPosition(position: Position): Boolean =
        0 <= position.row && position.row < height && 0 <= position.col && position.col < width


      def elem(position: Position): Option[Elem] =
        if (validPosition(position))
          Some(pattern(position.row)(position.col))
        else None

      def updated(pos: Position, elem: Elem): Grid = {
        val v = pattern(pos.row).updated(pos.col, elem)
        Grid(pattern.updated(pos.row, v))
      }

      implicit def elemToCount(elem: Option[Elem]): Long = elem match {
        case Some(Occupied) => 1L
        case _ => 0L
      }

      def countOccupiedNeighbours(pos: Position): Long =
        pos.neighbours map {p => elemToCount(elem(p))} sum


      def countOccupiedInSight(pos: Position): Long = {
        val seatsInSight = getAllSeatsInSight(pos)
        seatsInSight map {seat => elemToCount(seat)} sum
      }

      def transformedElemForPosition(pos: Position): Elem = {
        val numOfOccupiedNeighbours = countOccupiedNeighbours(pos)
        this.elem(pos) match {
          case Some(Empty) if (numOfOccupiedNeighbours == 0) => Occupied
          case Some(Occupied) if (numOfOccupiedNeighbours >= threshold) => Empty
          case Some(value) => value
          case None => throw new Exception("transforming None")
        }
      }

      // turn all this into transformers, outside grid
      def transformedElem2(pos: Position): Elem = {
        val numOfOccupiedInSight = countOccupiedInSight(pos)
        this.elem(pos) match {
          case Some(Empty) if (numOfOccupiedInSight == 0) => Occupied
          case Some(Occupied) if (numOfOccupiedInSight >= 5) => Empty
          case Some(value) => value
          case None => throw new Exception("transforming None")
        }

      }

      def transformed: Grid = {
        val newPattern =
          for ((v, r) <- pattern.zipWithIndex)
            yield for ((el, c) <- v.zipWithIndex)
              yield transformedElemForPosition(Position((r, c)))
        Grid(newPattern)
      }

      def transformed2: Grid = {
        val newPattern =
          for ((v, r) <- pattern.zipWithIndex)
            yield for ((el, c) <- v.zipWithIndex)
              yield transformedElem2(Position((r, c)))
        Grid(newPattern)
      }

      def getSeatInSight(position: Position, direction: Direction): Option[Seat] = {
        @tailrec
        def go(pos: Position): Option[Seat] = {
          val nextPos = pos.next(direction)
          elem(nextPos) match {
            case None => None
            case Some(Empty) => Some(Empty)
            case Some(Occupied) => Some(Occupied)
            case Some(Floor) => go(nextPos)
          }
        }
        go(position)
      }

      def getAllSeatsInSight(position: Position): List[Option[Seat]] = {
        val directions: List[Direction] = List(North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest)
        directions
          .map(direction => getSeatInSight(position, direction))
      }

      def countAllOccupied: Long = {
       val res = pattern map {v => v.count { el => el == Occupied}}
       res.sum.toLong
      }
    }

    object Grid {
      def apply(dataPath: String): Grid = Grid(readGridPattern(dataPath))
    }
  }

  object input {
    import domain._
    implicit def charToElem(z: Char): Elem = z match {
      case '.' => Floor
      case 'L' => Empty
      case '#' => Occupied
      case _ => throw new IllegalArgumentException("Unknown char.")
    }

    def readGridPattern(filePath: String): Vector[Vector[Elem]] = {
      val ss = readElemsNewLine(filePath)
      ss.map(_.toVector.map(charToElem)).toVector
    }
  }

  object part1 {
    import domain._

    def solution(grid: Grid): Long = {
      @tailrec
      def go(gr: Grid): Long = {
        val newGrid = gr.transformed
        if (newGrid == gr) {
          gr.countAllOccupied
        }
        else {
          go(newGrid)
        }
      }
      go(grid)
    }
  }

  object part2 {
    import domain._

    def solution(grid: Grid): Long = {
      @tailrec
      def go(gr: Grid): Long = {
        val newGrid = gr.transformed2
        if (newGrid == gr) {
          gr.countAllOccupied
        }
        else {
          go(newGrid)
        }
      }
      go(grid)
    }
  }

}

object Day11App extends App {
  import Day11._
  import domain._

  val dataPath = "src/main/resources/input/day11.in"
  val testDataPath = "src/main/resources/input/day11.test"

  val testGrid = Grid(testDataPath)
  val testSol1 = part1.solution(testGrid)
  println(s"Solution for test grid is $testSol1")

  val grid = Grid(dataPath)
  val sol1 = part1.solution(grid)
  println(s"Solution for part 1 is $sol1.") // 2361

  val sol2 = part2.solution(grid)
  println(s"Solution for part 2 is $sol2.") // 2119




}

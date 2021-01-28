package solutions

import scala.annotation.tailrec

object Day05 {
  object domain {
    sealed trait FrontBack
    case object Front extends FrontBack
    case object Back extends FrontBack

    sealed trait LeftRight
    case object Left extends LeftRight
    case object Right extends LeftRight

    case class Partition(frontBackPartition: List[FrontBack], leftRightPartition: List[LeftRight])

    case class Seat(row: Int, column: Int)
  }

  object input {
    import utils.utils.readElemsNewLine
    import domain._

    def parseFrontBack(z: Char): FrontBack = z match {
      case 'F' => Front
      case 'B' => Back
    }

    def parseLeftRight(z: Char): LeftRight = z match {
      case 'L' => Left
      case 'R' => Right
    }

    def readPartitionsFromFile(filePath: String,
                               frontBackLength: Int = 7,
                               leftRightLength: Int = 3): List[Partition] = {
      val vs: List[String] = readElemsNewLine(filePath).toList
      val vss: List[List[Char]] = vs.map(_.toList)
      val ps: List[(List[Char], List[Char])] = vss.map(_.splitAt(frontBackLength))
      val partitions: List[Partition] = ps.map(p => {
        val fbs = p._1.map(parseFrontBack)
        val lrs = p._2.map(parseLeftRight)
        Partition(fbs, lrs)
      })

      partitions
    }
  }

  object solutions {
    import domain._

    def lowerHalf(lo: Int, hi: Int): Int = lo + (hi - lo) / 2

    def upperHalf(lo: Int, hi: Int): Int = lo + math.ceil((hi - lo).toDouble / 2).toInt

    def decodeRow(frontBacks: List[FrontBack], low: Int = 0, high: Int = 127): Int = {
      @tailrec
      def go(fbs: List[FrontBack], lo: Int, hi: Int): Int = fbs match {
        case Nil => lo
        case fb :: fbs1 => fb match {
          case Front => go(fbs1, lo, lowerHalf(lo, hi))
          case Back => go(fbs1, upperHalf(lo, hi), hi)
        }
      }
      go(frontBacks, low, high)
    }

    def decodeCol(leftRights: List[LeftRight], low: Int = 0, high: Int = 7): Int = {
      @tailrec
      def go(lrs: List[LeftRight], lo: Int, hi: Int): Int = lrs match {
        case Nil => lo
        case lr :: lrs1 => lr match {
          case Left => go(lrs1, lo, lowerHalf(lo, hi))
          case Right => go(lrs1, upperHalf(lo, hi), hi)
        }
      }
      go(leftRights, low, high)
    }

    def decodeSeat(partition: Partition): Seat = {
      val row = decodeRow(partition.frontBackPartition)
      val col = decodeCol(partition.leftRightPartition)
      Seat(row, col)
    }

    def seatId(seat: Seat): Long = seat.row.toLong * 8 + seat.column

    object part1 {
      def solution(partitions: List[Partition]): Long = {
        val seats = partitions.map(decodeSeat)
        val seatIds: List[Long] = seats.map(seatId)
        seatIds.max
      }
    }

    object part2 {
      def solution(partitions: List[Partition]): Option[Long] = {
        @tailrec
        def go(as: List[Long], expected: Long): Option[Long] = as match {
          case Nil => None
          case a :: as1 => if (a != expected) Some(expected) else go(as1, expected + 1)
        }
        val seats = partitions.map(decodeSeat)
        val seatIds: List[Long] = seats.map(seatId)
        val sortedSeatIds = seatIds.sorted
        go(sortedSeatIds, sortedSeatIds.head)
      }
    }
  }
}

object Day05App extends App {
  import Day05.input._
  import Day05.domain._
  import Day05.solutions._

  val dataPath = "src/main/resources/input/day05.in"
  val testPath = "src/main/resources/input/day05.test"

  val partitions = readPartitionsFromFile(dataPath)
  val sol1 = part1.solution(partitions)
  val sol2 = part2.solution(partitions)
  println(s"Solution for part 1 is $sol1.\nSolution for part 2 is $sol2.")
  /** Correct ans:
   * Solution for part 1 is 963.
   * Solution for part 2 is Some(592).
   */

}

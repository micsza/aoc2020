package solutions

import fastparse._
import SingleLineWhitespace._
import solutions.Day24.domain.Direction
import utils.utils.readElemsNewLine

import scala.annotation.tailrec

object Day24 {

  object domain {
    sealed trait Color
    case object Black extends Color
    case object White extends Color

    case class Tile(color: Color) {
      def flip: Tile = this.color match {
        case Black => Tile(White)
        case White => Tile(Black)
      }
    }

    sealed trait Direction
    case object East extends Direction
    case object West extends Direction
    case object SouthEast extends Direction
    case object SouthWest extends Direction
    case object NorthEast extends Direction
    case object NorthWest extends Direction

    case class Position(se: Int, ne: Int) {
      def moveTo(direction: Direction): Position =
        direction match {
          case East => Position(se + 1, ne + 1)
          case West => Position(se - 1, ne - 1)
          case SouthEast => Position(se + 1, ne)
          case SouthWest => Position(se, ne - 1)
          case NorthEast => Position(se, ne + 1)
          case NorthWest => Position(se - 1, ne)
        }

      def getNeighbours: List[Position] =
        List(
          moveTo(East),
          moveTo(West),
          moveTo(SouthEast),
          moveTo(SouthWest),
          moveTo(NorthEast),
          moveTo(NorthWest)
        )
    }

    case class Grid(grid: Map[Position, Tile]) {

      def countAllBlack: Long = {
        this.grid.toList.count(_._2.color == Black)
      }

      def expandAround(pos: Position): Grid = {
        val ns: List[Position] = pos.getNeighbours
        val updatedGrid = ns.foldLeft(this.grid){ case (gr, pos) =>
          if (!(gr contains pos)) gr.updated(pos, Tile(White))
          else gr
        }
        Grid(updatedGrid)
      }

      def expand(): Grid = {
        this.grid.foldLeft(this){ case (g, (p, t)) => g.expandAround(p) }
      }

      def flip(position: Position): Grid = {
        if (grid contains position) {
          grid(position).color match {
            case White =>
              Grid(grid.updated(position, Tile(Black))).expandAround(position)
            case Black =>
              Grid(grid.updated(position, Tile(White))).expandAround(position)
          }
        }
        else {
          Grid(grid.updated(position, Tile(Black))).expandAround(position)
        }
      }

      def countBlackAround(position: Position): Long = {
        position
          .getNeighbours
          .map { pos =>
            this.grid.get(pos) match {
              case Some(Tile(Black)) => 1L
              case _ => 0L
            }
          }
          .sum
      }

      def evolveTileAt(position: Position): Tile = {
        def numBlackARound = countBlackAround(position)
//        println(s"Evolving at pos $position: # blacks around $numBlackARound")
        this.grid(position) match {
          case Tile(White) =>
            if (numBlackARound == 2) {
//              println(s"\t white ~> black")
              Tile(Black)
            }
            else {
//              println(s"\t stays white")
              Tile(White)
            }
          case Tile(Black) =>
            if (numBlackARound == 0 || numBlackARound > 2) {
//              println(s"\t black ~> white")
              Tile(White)
            }
            else {
//              println(s"\t stays black")
              Tile(Black)
            }
        }
      }

      def evolve(): Grid = {
        val g = this.grid.foldLeft(Map.empty: Map[Position, Tile]) { case (m, (p, t)) =>
          m.updated(p, evolveTileAt(p))
        }
        Grid(g).expand()
      }

      def printBlackPoss: Unit = {
        println("** Black positions:")
        this.grid.foreach {
          case (p, t) => t match {
            case Tile(White) => ()
            case Tile(Black) => println(p)
          }
        }
      }

    }

  }

  object parser {
    import domain._

    def parseMoves[_: P]: P[List[Direction]] = {
      P( ("e".! | "w".! | "se".! | "sw".! | "ne".! | "nw".!).rep ~ End)
        .map(_.map {
          case "e" => East
          case "w" => West
          case "nw" => NorthWest
          case "ne" => NorthEast
          case "se" => SouthEast
          case "sw" => SouthWest
        }.toList)
    }

    def parseLine(line: String): List[Direction] = {
      fastparse.parse(line, parseMoves(_)) match {
        case Parsed.Success(result, _) => result.toList
        case failure: Parsed.Failure => throw new RuntimeException(failure.trace().longMsg)
      }
    }

  }

  object input {
    def readInput(filePath: String): List[List[Direction]] = {
      val lines = readElemsNewLine(filePath).toList
      lines.map(parser.parseLine)
    }
  }

  object part1 {
    import domain._

    def dirs2Pos(ds: List[Direction]): Position =
      ds.foldLeft(Position(0,0)){ case (pos, d) => pos.moveTo(d)}

    def preprocess(dss: List[List[Direction]]): Grid = {
      val grid = dss.foldLeft(Map.empty: Map[Position, Tile]){ case (m, ds) =>
        val pos = dirs2Pos(ds)
        if (m contains pos) m.updated(pos, m(pos).flip)
        else m.updated(pos, Tile(Black))
      }
      Grid(grid)
    }

    def solution(dss: List[List[Direction]]): Int = {
      val grid = preprocess(dss)
      grid.grid.toList.count(_._2.color == Black)

    }
  }

  object part2 {
    import domain._

    def solution(dss: List[List[Direction]], n: Int): Long = {

      @tailrec
      def go(m: Int, gr: Grid): Grid =
        if (m > 0) {
          println(s"\n\n-- day ${n - m}: # blacks = ${gr.countAllBlack}")
//          gr.grid foreach println
//          gr.printBlackPoss
          go(m - 1, gr.evolve())
        }
        else gr

      val grid0 = part1.preprocess(dss).expand()
      val gridN = go(n, grid0)
      gridN.countAllBlack
    }
  }
}

object Day24App extends App {
  import Day24._

  val puzzleDataPath = "src/main/resources/input/day24.in"
  val testDataPath = "src/main/resources/input/day24.test"

  val testInput = input.readInput(testDataPath)
  val puzzleInput = input.readInput(puzzleDataPath)

//  val testSol1 = part1.solution(testInput)
//  println(s"Test solution for part 1 is $testSol1.")



  val sol1 = part1.solution(puzzleInput)
  println(s"Solution for part 1 is $sol1.")


//  val testSol2 = part2.solution(testInput, 100)
//  println(s"Test sol 2 = $testSol2.")

  val sol2 = part2.solution(puzzleInput, 100)
  println(s"Solution for part 2 is $sol2.")

}

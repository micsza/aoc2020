package solutions

import utils.utils.readElemsNewLine
import scala.language.postfixOps

object Day12 {

  object domain1 {
    trait Action

    trait Move extends Action
    case class MoveNorth(value: Int) extends Move
    case class MoveSouth(value: Int) extends Move
    case class MoveWest(value: Int) extends Move
    case class MoveEast(value: Int) extends Move
    case class MoveForward(value: Int) extends Move

    trait Rotation extends Action
    case class RotateLeft(tick: RotationTick) extends Rotation
    case class RotateRight(tick: RotationTick) extends Rotation

    trait RotationTick
    case object TickZero extends RotationTick
    case object TickOne extends RotationTick
    case object TickTwo extends RotationTick
    case object TickThree extends RotationTick

    trait Direction {
      def rotateBy(rotation: Rotation): Direction
    }
    case object North extends Direction {
      def rotateBy(rotation: Rotation): Direction =
        rotation match {
          case RotateLeft(tick) => tick match {
            case TickZero => North
            case TickOne => West
            case TickTwo => South
            case TickThree => East
          }
          case RotateRight(tick) => tick match {
            case TickZero => North
            case TickOne => East
            case TickTwo => South
            case TickThree => West
          }
        }
    }
    case object South extends Direction {
      def rotateBy(rotation: Rotation): Direction =
        rotation match {
          case RotateLeft(tick) => tick match {
            case TickZero => South
            case TickOne => East
            case TickTwo => North
            case TickThree => West
          }
          case RotateRight(tick) => tick match {
            case TickZero => South
            case TickOne => West
            case TickTwo => North
            case TickThree => East
          }
        }
    }
    case object East extends Direction {
      def rotateBy(rotation: Rotation): Direction =
        rotation match {
          case RotateLeft(tick) => tick match {
            case TickZero => East
            case TickOne => North
            case TickTwo => West
            case TickThree => South
          }
          case RotateRight(tick) => tick match {
            case TickZero => East
            case TickOne => South
            case TickTwo => West
            case TickThree => North
          }
        }
    }
    case object West extends Direction {
      def rotateBy(rotation: Rotation): Direction =
        rotation match {
          case RotateLeft(tick) => tick match {
            case TickZero => West
            case TickOne => South
            case TickTwo => East
            case TickThree => North
          }
          case RotateRight(tick) => tick match {
            case TickZero => West
            case TickOne => North
            case TickTwo => East
            case TickThree => South
          }
        }
    }

    case class Position(east: Int, north: Int)

    case class Ship(position: Position, direction: Direction) {
      def move(action: Action): Ship = {
        println(s"\n---- Ship on the move ---\n\tfrom: $this\n\tby: $action")
        val ship = action match {
          case MoveNorth(value) => Ship(Position(position.east, position.north + value), direction)
          case MoveSouth(value) => Ship(Position(position.east, position.north - value), direction)
          case MoveEast(value) => Ship(Position(position.east + value, position.north), direction)
          case MoveWest(value) => Ship(Position(position.east - value, position.north), direction)
          case MoveForward(value) => direction match {
            case North => this.move(MoveNorth(value))
            case South => this.move(MoveSouth(value))
            case East => this.move(MoveEast(value))
            case West => this.move(MoveWest(value))
          }
          case RotateLeft(tick) => Ship(position, direction.rotateBy(RotateLeft(tick)))
          case RotateRight(tick) => Ship(position, direction.rotateBy(RotateRight(tick)))
        }
        println(s"\tto: $ship")
        ship
      }
    }
  }

  object domain2 {

    trait Action
    case class MoveNorth(value: Int) extends Action
    case class MoveSouth(value: Int) extends Action
    case class MoveWest(value: Int) extends Action
    case class MoveEast(value: Int) extends Action
    case class MoveForward(value: Int) extends Action
    case class RotateLeft(tick: RotationTick) extends Action // counter-clockwise
    case class RotateRight(tick: RotationTick) extends Action // clockwise

    case class Position(east: Int, north: Int) {
      def move(move: Action): Position = move match {
        case MoveNorth(value) => Position(this.east, this.north + value)
        case MoveSouth(value) => Position(this.east, this.north - value)
        case MoveEast(value) => Position(this.east + value, this.north)
        case MoveWest(value) => Position(this.east - value, this.north)
        case RotateLeft(tick) => tick match {
          case TickZero => this
          case TickOne => Position(-this.north, this.east)
          case TickTwo => Position(-this.east, -this.north)
          case TickThree => Position(this.north, -this.east)
        }
        case RotateRight(tick) => tick match {
          case TickZero => this
          case TickOne => Position(this.north, -this.east)
          case TickTwo => Position(-this.east, -this.north)
          case TickThree => Position(-this.north, this.east)
        }
      }
    }

    trait RotationTick
    case object TickZero extends RotationTick
    case object TickOne extends RotationTick
    case object TickTwo extends RotationTick
    case object TickThree extends RotationTick

    case class Waypoint(point: Position) {
      def action(move: Action): Waypoint = Waypoint(point.move(move))
    }

    case class Ship(position: Position, waypoint: Waypoint) {
      def action(action: Action): Ship = {
        println(s"\n---- Ship on the move ---\n\tfrom: $this\n\tby: $action")
        val ship = action match {
          case MoveNorth(value) => Ship(position, waypoint.action(MoveNorth(value)))
          case MoveSouth(value) => Ship(position, waypoint.action(MoveSouth(value)))
          case MoveEast(value) => Ship(position, waypoint.action(MoveEast(value)))
          case MoveWest(value) => Ship(position, waypoint.action(MoveWest(value)))
          case RotateLeft(tick) => Ship(position, waypoint.action(RotateLeft(tick)))
          case RotateRight(tick) => Ship(position, waypoint.action(RotateRight(tick)))

          case MoveForward(value) =>
            Ship(
              Position(this.position.east + waypoint.point.east * value, this.position.north + waypoint.point.north * value),
              waypoint
            )
        }
        println(s"\tto: $ship")

        ship
      }
    }
  }

  object input2 {
    import domain2._

    def parseTick(num: Int): RotationTick = num match {
      case 0 => TickZero
      case 90 => TickOne
      case 180 => TickTwo
      case 270 => TickThree
      case other => throw new Exception(s"Unknown tick: $other")
    }

    def parseAction(string: String): Action = {
      val actionCode: Char = string(0)
      val num: Int = string.drop(1).toInt

      actionCode match {
        case 'N' => MoveNorth(num)
        case 'S' => MoveSouth(num)
        case 'E' => MoveEast(num)
        case 'W' => MoveWest(num)
        case 'L' => RotateLeft(parseTick(num))
        case 'R' => RotateRight(parseTick(num))
        case 'F' => MoveForward(num)
        case other => throw new Exception(s"Unknown action code: $other.")
      }

    }
    def readInput(filePath: String): List[Action] =
      readElemsNewLine(filePath) map parseAction toList


  }

  object input1 {
    import domain1._

    def parseTick(num: Int): RotationTick = num match {
        case 0 => TickZero
        case 90 => TickOne
        case 180 => TickTwo
        case 270 => TickThree
        case other => throw new Exception(s"Unknown tick: $other")
    }

    def parseAction(string: String): Action = {
      val actionCode: Char = string(0)
      val num: Int = string.drop(1).toInt

      actionCode match {
        case 'N' => MoveNorth(num)
        case 'S' => MoveSouth(num)
        case 'E' => MoveEast(num)
        case 'W' => MoveWest(num)
        case 'L' => RotateLeft(parseTick(num))
        case 'R' => RotateRight(parseTick(num))
        case 'F' => MoveForward(num)
        case other => throw new Exception(s"Unknown action code: $other.")
      }

    }
    def readInput(filePath: String): List[Action] =
      readElemsNewLine(filePath) map parseAction toList

  }

  object part1 {
    import domain1._
    def solution(actions: List[Action]): Int = {
      val initialShip = Ship(Position(0, 0), East)
      val finalShip = actions.foldLeft(initialShip)((ship, action) => ship.move(action))
      math.abs(finalShip.position.north) + math.abs(finalShip.position.east)
    }
  }

  object part2 {
    import domain2._

    def solution(actions: List[Action]): Int = {
      val initialShip = Ship(Position(0, 0), Waypoint(Position(10, 1)))
      val finalShip = actions.foldLeft(initialShip)((ship, action) => ship.action(action))
      math.abs(finalShip.position.north) + math.abs(finalShip.position.east)
    }
  }

}

object Day12App extends App {
  import Day12._

  val inputPath = "src/main/resources/input/day12.in"
  val testPath = "src/main/resources/input/day12.test"
  val test2Path = "src/main/resources/input/day12_2.test"

  val testActions = input1.readInput(testPath)
  val actions = input1.readInput(inputPath)

  val sol1 = part1.solution(actions)
  println(s"Solution for part 1 is $sol1.")

  {
    val actions = input2.readInput(inputPath)
    val sol2 = part2.solution(actions)
    println(s"Solution for part 2 is $sol2")
  }

}

package solutions


object Day17 {

  object part1 {
    object domain {
      trait Cube
      case object Active extends Cube
      case object NotActive extends Cube


      case class Point(x: Int, y: Int, z: Int) {
        def leftOf: Point = Point(this.x - 1, this.y, this.z)
        def rightOf: Point = Point(this.x + 1, this.y, this.z)
        def upOf: Point = Point(this.x, this.y + 1, this.z)
        def downOf: Point = Point(this.x, this.y - 1, this.z)
        def frontOf: Point = Point(this.x, this.y, this.z - 1)
        def backOf: Point = Point(this.x, this.y, this.z + 1)
      }

      case class Size3D(xSize: Int, ySize: Int, zSize: Int) {

        def xShift: Int = (xSize - 1) / 2
        def yShift: Int = (ySize - 1) / 2
        def zShift: Int = (zSize - 1) / 2

        def points: List[Point] = {
          val ps =
            for {
              i <- (0 to xSize).map(_ - xShift)
              j <- (0 to ySize).map(_ - yShift)
              k <- (0 to zSize).map(_ - zShift)
            } yield Point(i, j, k)
          ps.toList
        }

        def inc: Size3D = Size3D(xSize + 1, ySize + 1, zSize + 1)
      }


      type Region = Map[Point, Cube]

      case class Grid(region: Region, size: Size3D) {

        def cube(point: Point): Cube =
          region.get(point) match {
            case Some(value) => value
            case None => NotActive
          }

        def neighborPoints(point: Point): List[Point] = {
          val ns =
            for {
              i <- point.x - 1 to point.x + 1
              j <- point.y - 1 to point.y + 1
              k <- point.z - 1 to point.z + 1 if Point(i, j, k) != point
            } yield Point(i, j, k)

          ns.toList
        }

        def neighborCubes(point: Point): List[Cube] =
          neighborPoints(point).map(cube)

        def evolveCubeAt(point: Point): Cube = {
          val c = cube(point)
          val ncs = neighborCubes(point)
          val numOfActive = ncs.count(_ == Active)
          c match {
            case Active =>
              if (numOfActive == 2 || numOfActive == 3) Active
              else NotActive
            case NotActive =>
              if (numOfActive == 3) Active
              else NotActive
          }
        }

        def evolve: Grid = {
          def evolvedSize = this.size.inc.inc
          def evolvedPoints = evolvedSize.points
          def evolvedRegion = evolvedPoints.foldLeft[Region](Map.empty) { case (m, p) => m.updated(p, evolveCubeAt(p))}
          Grid(evolvedRegion, evolvedSize)
        }

      }
    }

    object input {
      import part1.domain._
      import utils.utils.readElemsNewLine

      implicit def charToCube(z: Char): Cube = z match {
        case '.' => NotActive
        case '#' => Active
        case _ => throw new IllegalArgumentException("Unknown char.")
      }

      def readGrid(dataPath: String): Grid = {
        val strings: Vector[(String, Int)] = readElemsNewLine(dataPath).toVector.zipWithIndex
        val size = Size3D(strings.head._1.length, strings.length, 1)

        println(s"-- SIZE = $size")

        val yShiftUp = 4 // TODO !!!! this shouldnt be fixed
        val plain: Vector[(Point, Cube)] = strings.flatMap { case (s, j) =>
          s.toVector.zipWithIndex.map { case (z, i) =>
            (Point(i - size.xShift, yShiftUp - j, 0), charToCube(z))
          }
        }
        plain.foreach(println)

        Grid(plain.toMap, size)
      }

    }

    import domain._
    def solution(grid: Grid, rounds: Int = 6): Long = {

      def go(gr: Grid, roundLeft: Int): Grid = {
        if (roundLeft == 0) gr
        else go(gr.evolve, roundLeft - 1)
      }

      val endGrid = go(grid, rounds)
      println(s"--- end grid after $rounds rounds:\n")
      endGrid.region.foreach(println)

      endGrid.region.count { case (p, c) => c == Active}
    }
  }

  object part2 {
    object domain {
      trait Cube
      case object Active extends Cube
      case object NotActive extends Cube

      case class Point4D(x: Int, y: Int, z: Int, w: Int)

      case class Size4D(xSize: Int, ySize: Int, zSize: Int, wSize: Int) {

        def xShift: Int = (xSize - 1) / 2
        def yShift: Int = (ySize - 1) / 2
        def zShift: Int = (zSize - 1) / 2
        def wShift: Int = (wSize - 1) / 2

        def points: List[Point4D] = {
          val ps =
            for {
              i <- (0 to xSize).map(_ - xShift)
              j <- (0 to ySize).map(_ - yShift)
              k <- (0 to zSize).map(_ - zShift)
              l <- (0 to wSize).map(_ - wShift)
            } yield Point4D(i, j, k, l)
          ps.toList
        }

        def inc: Size4D = Size4D(xSize + 1, ySize + 1, zSize + 1, wSize + 1)
      }

      type Region4D = Map[Point4D, Cube]


      case class Grid4D(region: Region4D, size: Size4D) {
        def cube(point: Point4D): Cube =
          region.get(point) match {
            case Some(value) => value
            case None => NotActive
          }

        def neighborPoints(point: Point4D): List[Point4D] = {
          val ns =
            for {
              i <- point.x - 1 to point.x + 1
              j <- point.y - 1 to point.y + 1
              k <- point.z - 1 to point.z + 1
              l <- point.w - 1 to point.w + 1 if Point4D(i, j, k, l) != point
            } yield Point4D(i, j, k, l)

          ns.toList
        }

        def neighborCubes(point: Point4D): List[Cube] =
          neighborPoints(point).map(cube)

        def evolveCubeAt(point: Point4D): Cube = {
          val c = cube(point)
          val ncs = neighborCubes(point)
          val numOfActive = ncs.count(_ == Active)
          c match {
            case Active =>
              if (numOfActive == 2 || numOfActive == 3) Active
              else NotActive
            case NotActive =>
              if (numOfActive == 3) Active
              else NotActive
          }
        }

        def evolve: Grid4D = {
          def evolvedSize: Size4D = this.size.inc.inc
          def evolvedPoints: List[Point4D] = evolvedSize.points
          def evolvedRegion: Region4D = evolvedPoints.foldLeft[Region4D](Map.empty) { case (m, p) => m.updated(p, evolveCubeAt(p))}
          Grid4D(evolvedRegion, evolvedSize)
        }
      }
    }

    object input {
      import part2.domain._
      import utils.utils.readElemsNewLine

      implicit def charToCube(z: Char): Cube = z match {
        case '.' => NotActive
        case '#' => Active
        case _ => throw new IllegalArgumentException("Unknown char.")
      }

      def readGrid(dataPath: String): Grid4D = {
        val strings: Vector[(String, Int)] = readElemsNewLine(dataPath).toVector.zipWithIndex
        val size = Size4D(strings.head._1.length, strings.length, 1, 1)

        println(s"-- SIZE = $size")

        val yShiftUp = 4 // TODO !!!! this shouldnt be fixed
        val plain: Vector[(Point4D, Cube)] = strings.flatMap { case (s, j) =>
          s.toVector.zipWithIndex.map { case (z, i) =>
            (Point4D(i - size.xShift, yShiftUp - j, 0, 0), charToCube(z))
          }
        }
        plain.foreach(println)

        Grid4D(plain.toMap, size)
      }
    }

    import domain._
    def solution(grid: Grid4D, rounds: Int = 6): Long = {

      def go(gr: Grid4D, roundLeft: Int): Grid4D = {
        if (roundLeft == 0) gr
        else go(gr.evolve, roundLeft - 1)
      }

      val endGrid = go(grid, rounds)
      println(s"--- end grid after $rounds rounds:\n")
      endGrid.region.foreach(println)

      endGrid.region.count { case (p, c) => c == Active}
    }

  }



}

object Day17App extends App {

  import Day17._

  val dataPath = "src/main/resources/input/day17.in"
  val testPath = "src/main/resources/input/day17.test"


  val grid = part2.input.readGrid(dataPath)
  val sol2 = part2.solution(grid)
  println(s"Solution for part 2 is $sol2.")

}

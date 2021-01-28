package solutions

import utils.utils.readElemsSeparator
import utils.utils.readElemsNewLine

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day13 {

  object part1 {

    object domain {
      case class Bus(id: Long, delta: Long) {
        def departsAtTime(time: Long): Boolean = time % id == 0
      }
    }

    object input {
      import domain._

      def parseElem(elem: String): Option[Long] = {
        val number: Regex = """(\d+)""".r
        elem match {
          case number(a) => Some(a.toLong)
          case _ => None
        }
      }

      def toDomain(longs: List[Option[Long]]): List[Bus] = {
        @tailrec
        def go(ls: List[Option[Long]], busAcc: List[Bus], deltaAcc: Long): List[Bus] = {
          ls match {
            case Nil => busAcc
            case l :: ls1 =>
              l match {
                case Some(id) => go(ls1, Bus(id, deltaAcc + 1) :: busAcc, 0)
                case None => go(ls1, busAcc, deltaAcc + 1)
              }
          }
        }
        go(longs, Nil, -1).reverse
      }

      def readInput(filepath: String): List[Bus] = {
        val strings: List[String] = readElemsNewLine(filepath).toList
        val timestamp: Long = strings.head.toLong
        val rest: List[String] = strings(1).split(",").toList
        val longs: List[Option[Long]] = rest.map(parseElem)
        val buses = toDomain(longs)
        println(s"buses: $buses")
        buses
      }
    }
  }


  object part2 {

    object domain {

      type Time = Long

      case class Bus(id: Time, delta: Time) {

        def departsAtTime(time: Time): Boolean = time % id == 0

        def findMatch(bundleMatch: Time, bundleLcm: Time): Time = {
          val newLcm = lcm(this.id, bundleLcm)
          val busStart = math.max((bundleMatch / this.id) * this.id - delta, this.id - delta)
          println(s"\t* FIND MATCH for bus $this with bus start = $busStart, bundled match = $bundleMatch, bundled lcm = $bundleLcm, new lcm = $newLcm")


          @tailrec
          def go(busTime: Time, busJump: Time, bundleTime: Time, bundleJump: Time): Time = {
//            Thread.sleep(100)
            val newBundleTime = bundleTime + bundleJump * bundleLcm
            val newBusTime = busTime + busJump * id
            println(s"\t\t** GO: new bundled time $newBundleTime, new bus time $newBusTime")

            if ( newBusTime < newBundleTime ) {
              val newBusJump = math.max((newBundleTime - newBusTime) / id, 1)
              println(s"\t\t\tbus jump: new bus time $newBusTime, new bundle time $newBundleTime => jump bus by $newBusJump")
              go(newBusTime, newBusJump, newBundleTime, 0L)
            }
            else {
              if (newBusTime > newBundleTime) {
                println(s"\t\tbundle jump up by 1")
                go(newBusTime, 0L, newBundleTime, 1L)
              }
              else newBusTime
            }
          }

          go(busStart, 0L, bundleMatch, 0L)
        }
      }

      def lcm(a: Time, b: Time): Time = a * b / (BigInt(a).gcd(b)).toLong
    }

    object input {
      import domain._

      def parseElem(elem: String): Option[Time] = {
        val number: Regex = """(\d+)""".r
        elem match {
          case number(a) => Some(a.toLong)
          case _ => None
        }
      }

      def toDomain(longs: List[Option[Time]]): List[Bus] = {
        @tailrec
        def go(ls: List[Option[Time]], busAcc: List[Bus], deltaAcc: Time): List[Bus] = {
          ls match {
            case Nil => busAcc
            case l :: ls1 =>
              l match {
                case Some(id) => go(ls1, Bus(id, deltaAcc) :: busAcc, deltaAcc + 1)
                case None => go(ls1, busAcc, deltaAcc + 1)
              }
          }
        }
        go(longs, Nil, 0L).reverse
      }

      def readInput(filepath: String): List[Bus] = {
        val strings: List[String] = readElemsNewLine(filepath).toList
        val timestamp: Long = strings.head.toLong
        val rest: List[String] = strings(1).split(",").toList
        val longs: List[Option[Time]] = rest.map(parseElem)
        val buses = toDomain(longs)
        println(s"buses: $buses")
        buses
      }

    }

    object solution {
      import domain._

      def solution(buses: List[Bus]): Time = {
        @tailrec
        def go(bs: List[Bus], bundledLcm: Time, timestamp: Time): Time = {

          bs match {
            case Nil => timestamp
            case b :: bs1 =>
              println(s"------------------ bus to bundle: $b, bundled lcm: $bundledLcm, timestamp: $timestamp")
              val newLcm = lcm(b.id, bundledLcm)
              val newTimeStamp = b.findMatch(timestamp, bundledLcm)
              go(bs1, newLcm, newTimeStamp)
          }
        }

        go(buses, buses.head.id, 0L)
      }
    }
  }
}

object Day13App extends App {
  import Day13._

  val inputPath = "src/main/resources/input/day13.in"
  val testPath = "src/main/resources/input/day13.test"
  val test2Path = "src/main/resources/input/day13_2.test"

  import part2._

  val testData = input.readInput(testPath)
  println(testData)
  val testSol = solution.solution(testData)
  println(testSol)

  val inputData = input.readInput(inputPath)
  val sol = solution.solution(inputData)
  println(sol)


//  val inputData = input.readInput(inputPath)
//  val sol2 = part2.solution(inputData)
//  println(sol2)


}

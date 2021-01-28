package solutions

import solutions.Day14.domain.Expr
import utils.utils.readElemsNewLine

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.matching.Regex

object Day14 {

  object domain {
    trait Expr
    case class Mask(value: String) extends Expr
    case class Mem(address: Long, value: Long) extends Expr

    type Memory = Map[Long, Long]


  }

  object input {
    import utils.utils.readElemsSeparator
    import domain._

    def parseLine(line: String): Expr = {
      val maskRegex: Regex = """mask = ([01X]+)""".r
      val memRegex: Regex = """mem\[(\d+)\] = (\d+)""".r
      line match {
        case maskRegex(value) => Mask(value)
        case memRegex(addr, value) => Mem(addr.toLong, value.toLong)
        case _ => throw new Exception("Cannot parse.")
      }
    }

    def readInput(filePath: String): List[Expr] = {
      val strings = readElemsNewLine(filePath)
      strings map parseLine toList
    }
  }

  object part1 {
    import domain._

    def maskedValue(mask: Mask, value: Long): Long = {

      def maskedBit(bitChar: Char, maskChar: Char) = {
        maskChar match {
          case 'X' => bitChar
          case oth => oth
        }
      }
      val valueBitString: String = value.toBinaryString
      val valueBitStringExt = "0" * (36 - valueBitString.length) + valueBitString
      val zipped = valueBitStringExt zip mask.value
      val cs: List[(Char, Char)] = zipped.toList
      val masked: List[Char] = cs map {case (b, m) => maskedBit(b, m)}
      val str = masked.mkString

      BigInt(str, 2).toLong
    }

    def solution(exprs: List[Expr]): Long = {

      @tailrec
      def go(es: List[Expr], mask: Mask, memory: Memory): Memory = {
        es match {
          case Nil => memory
          case Mask(value) :: es1 => go(es1, Mask(value), memory)
          case Mem(addr, value) :: es1 =>
            val masked = maskedValue(mask, value)
            val updatedMemory = memory.updated(addr, masked)
            go(es1, mask, updatedMemory)
        }
      }

      val memory = go(exprs, Mask(""), Map.empty)
      memory.values.sum
    }
  }

  object part2 {
    import domain._

    def generateAddresses(mask: Mask, address: Long): List[Long] = {

      def maskedBit(bitChar: Char, maskChar: Char): Char = {
        maskChar match {
          case '0' => bitChar
          case oth => oth
        }
      }

      def applyFixedMask(mask: Mask, address: Long): List[Char]= {
        val addressBitString = address.toBinaryString
        val addressBitStringExt = "0" * (36 - addressBitString.length) + addressBitString
        val cs = addressBitStringExt zip mask.value toList

        cs map {case (addrBit, maskBit) => maskedBit(addrBit, maskBit) }
      }

      // recursive - there is max of 8 floating mask bits in each mask so we won't blow up the stack
      def go(floatingAddress: List[Char], fixedPrefix: List[Char]): List[List[Char]] = {
        floatingAddress match {
          case 'X' :: cs =>
            val zeroBranch = go(cs, '0' :: fixedPrefix)
            val oneBranch = go(cs, '1'   :: fixedPrefix)
            zeroBranch ++ oneBranch
          case c :: cs => go(cs, c :: fixedPrefix)
          case Nil => List(fixedPrefix.reverse)
        }
      }

      val flAddr: List[Char] = applyFixedMask(mask, address)
      val charAddresses: List[List[Char]] = go(flAddr, Nil)
      charAddresses.foreach(println(_))
      charAddresses.map(cs => BigInt(cs.mkString, 2).toLong)
//      List(1L)

    }

    def solution(exprs: List[Expr]): Long = {

      @tailrec
      def go(es: List[Expr], mask: Mask, memory: Memory): Memory = {
        es match {
          case Nil => memory
          case Mask(value) :: es1 => go(es1, Mask(value), memory)
          case Mem(addr, value) :: es1 =>
            val addresses: List[Long] = generateAddresses(mask, addr)
            val updatedMemory = addresses.foldLeft[Memory](memory) {
              case (m, a) => m.updated(a, value)
            }
            go(es1, mask, updatedMemory)
        }
      }

      val memory = go(exprs, Mask(""), Map.empty)
      memory.values.sum
    }
  }

}

object Day14App extends App {
  import Day14._
  import domain._

  val dataPath = "src/main/resources/input/day14.in"
  val testPath = "src/main/resources/input/day14.test"

//  val testData = input.readInput(testPath)
//  val testSol = part1.solution(testData)
//  println(s"Test sol = $testSol.")
//
  val inputData = input.readInput(dataPath)
  val sol1 = part1.solution(inputData)
  println(s"Solution for part 1 is $sol1.")

  val mask = Mask("000000000000000000000000000000X1001X")
  val addr = 42L
  println(part2.generateAddresses(mask, addr))

  val sol2 = part2.solution(inputData)
  println(s"Solution for part 2 is $sol2.")


}

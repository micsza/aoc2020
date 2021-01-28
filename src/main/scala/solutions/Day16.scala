package solutions

import fastparse._
import SingleLineWhitespace._

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

object Day16 {

  object part1 {
    object domain {
      type FieldValue = Int
      type FieldValueRange = (FieldValue, FieldValue)
      case class Field(name: String, range1: FieldValueRange, range2: FieldValueRange) {
        def fallsInRange(value: FieldValue): Boolean = {
          if ((range1._1 <= value && value <= range1._2) || (range2._1 <= value && value <= range2._2)) {
//            println(s"\t value [$value] in range for field [$this].")
            true
          }
          else false
        }
      }
      type EncodedTicket = List[FieldValue]


    }

    object Parser {
      import domain._

      case object EmptyLine
      case object YourTicketLine
      case object NearbyTicketsLine

      def fieldName[_: P]: P[String] = P(CharsWhileIn("a-z ").!)
      def int[_: P]: P[Int] = P(CharsWhileIn("0-9").!).map(_.toInt)
      def ints[_: P]: P[List[Int]] = P(int.rep(sep=",") ~ End) map {is => is.toList}
      def fieldRange[_: P]: P[(FieldValue, FieldValue)] = P(int ~ "-" ~ int) map {
        case (i, j) => (i, j)
      }
      def field[_: P]: P[Field] = P(fieldName ~ ":" ~ fieldRange ~ "or" ~ fieldRange ~ End) map {
        case (name, r1, r2) => Field(name, r1, r2)
      }

      def emptyLine[_: P]: P[EmptyLine.type] = P("".!) map (_ => EmptyLine)
      def yourTicketHeader[_: P]: P[YourTicketLine.type] = P("your ticket:") map (_ => YourTicketLine)
      def nearbyTicketsHeader[_: P]: P[NearbyTicketsLine.type] = P("nearby tickets:") map (_ => NearbyTicketsLine)

      def fieldOrEmptyLine[_: P]: P[Equals] = P(field | emptyLine)
    }

    object input {
      import utils.utils.readElemsNewLine
      import domain._
      def readInput(filepath: String): (List[Field], EncodedTicket, List[EncodedTicket]) = {

        def parseFields(lines: List[String]): (List[Field], List[String]) = {

          @tailrec
          def go(ls: List[String], fieldsAcc: List[Field]): (List[Field], List[String]) = {
            ls match {
              case l :: ls1 =>
//                println(s"-- [$l]")
                fastparse.parse(l, Parser.fieldOrEmptyLine(_)) match {
                  case Parsed.Success(result, _) =>
                    result match {
                      case Parser.EmptyLine => (fieldsAcc, ls1)
                      case field: Field => go(ls1, field :: fieldsAcc)
                    }
                  case failure: Parsed.Failure =>
                    throw new RuntimeException(failure.trace().longMsg)
                }
              case Nil => (fieldsAcc, Nil) // should never happen
            }
          }
          go(lines, Nil)
        }

        def parseNearbyTickets(lines: List[String]): List[EncodedTicket] = {
          def go(ls: List[String], ticketsAcc: List[EncodedTicket]): List[EncodedTicket] = {
            ls match {
              case Nil => ticketsAcc
              case l :: ls1 =>
                val ticket = fastparse.parse(l, part1.Parser.ints(_)) match {
                  case Parsed.Success(ticket, _) => ticket
                  case failure: Parsed.Failure => throw new RuntimeException(failure.trace().longMsg)
                }
                go(ls1, ticket :: ticketsAcc)

            }
          }
          go(lines, Nil)
        }

        val lines = readElemsNewLine(filepath)
        val (fields, rest) = parseFields(lines.toList)
        val yourTicketLine = rest(1)
        val nearbyTicketsLines = rest.drop(4)

        val yourTicket: EncodedTicket = fastparse.parse(yourTicketLine, part1.Parser.ints(_)) match {
          case Parsed.Success(ticket, _) => ticket
          case failure: Parsed.Failure => throw new RuntimeException(failure.trace().longMsg)
        }
        val nearbyTickets: List[EncodedTicket] = parseNearbyTickets(nearbyTicketsLines)
        (fields, yourTicket, nearbyTickets)

      }
    }

    import domain._
    def solution(tickets: List[EncodedTicket], fields: List[Field]): Long = {

      def sumOfTicketValuesInSomeRange(ticket: EncodedTicket): Int = {
        ticket.foldLeft[Int](0) { case (acc, fieldValue) =>
          val fieldValueInSomeRange: Boolean =
            fields.foldLeft[Boolean](false)((acc, field) => acc || field.fallsInRange(fieldValue))

          if (fieldValueInSomeRange) {
            println(s"Ticket [$ticket]: field value [$fieldValue] in some range.")
            acc
          }
          else {
            println(s"Ticket [$ticket]: field value [$fieldValue] in NONE of the ranges => adding.")
            acc+ fieldValue
          }
        }
      }

      def go(ts: List[EncodedTicket], acc: Long): Long = {
        ts match {
          case t :: ts1 =>
            val n = sumOfTicketValuesInSomeRange(t)
            go(ts1, acc + n)
          case Nil => acc
        }
      }
      go(tickets, 0L)
    }
  }

  object part2 {
    import part1.domain._

//    def isTicketValid(ticket: EncodedTicket, fields: List[Field]) = {
//      fields.forall(_.fallsInRange(ticket))
//    }
  }

}

object Day16App extends App {
  import Day16._
  import part1.domain._

  val fieldLine = "departure location: 28-184 or 203-952"
  val intsLine = "170,218,811,107,747,184,411,426,594,629,764,509,287,385,734,853,646,474,937,773"

  val parsedField = fastparse.parse(fieldLine, part1.Parser.field(_))
  println(parsedField)

  val parsedIntsLine = fastparse.parse(intsLine, part1.Parser.ints(_))
  println(parsedIntsLine)

  val parsedFieldOrEmpty = fastparse.parse(fieldLine, part1.Parser.fieldOrEmptyLine(_)) match {
    case Parsed.Success(result, _) =>
      result match {
        case ints: List[Int] => println(s"Parsed ints: $ints")
        case field: Field => println(s"Parsed field: $field")
      }
    case failure: Parsed.Failure => Failure(new RuntimeException(failure.trace().longMsg))
  }
  println(parsedFieldOrEmpty)

  val dataPath = "src/main/resources/input/day16.in"
  val testPath = "src/main/resources/input/day16.test"

  val (testFields, testTicket, testOtherTickets) = part1.input.readInput(testPath)
  println(s"fields: $testFields\nticket = $testTicket\nother tickets = $testOtherTickets")

  val testSol = part1.solution(testOtherTickets, testFields)
  println(s"Test osl = $testSol.")

  val (fields, ticket, otherTickets) = part1.input.readInput(dataPath)
  val sol1 = part1.solution(otherTickets, fields)
  println(s"Solution for part 1 is $sol1.")



}

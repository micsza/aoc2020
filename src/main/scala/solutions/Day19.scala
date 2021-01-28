package solutions

import fastparse._
import SingleLineWhitespace._
import utils.utils.readElemsNewLine

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Day19 {

  object domain {
    trait Rule {
      val id: Int
    }
    case class FixedRule(id: Int, value: String) extends Rule
    case class AndRule(id: Int, subRules: List[Int]) extends Rule
    case class XorRule(id: Int, subRules: List[List[Int]]) extends Rule

  }

  object parser {
    import domain._

    def number[_: P]: P[Int] = P( CharsWhileIn("0-9").! ).map(_.toInt)

    def ints[_: P]: P[List[Int]] = P(number.rep).map(_.toList)

    def word[_: P]: P[String] = P("\"" ~ CharsWhileIn("a-z").! ~ "\"")

    def parseNonFixedRule[_: P]: P[Rule] =
      P(number ~ ": " ~ ints ~ ("|" ~ ints).rep ~ End) map {
        case (i: Int, is: List[Int], iss: List[List[Int]]) => iss match {
          case Nil => AndRule(i, is)
          case _ => XorRule(i, is :: iss)
        }
      }

    def parseFixedRule[_: P]: P[Rule] =
      P(number ~ ":" ~ word ~ End) map {
        case (i, str) => FixedRule(i, str)
      }

    def parseRule[_: P]: P[Rule] =
      P(parseNonFixedRule | parseFixedRule)

    def parseRuleLine(line: String): Try[Rule] = {
      fastparse.parse(line, parseRule(_)) match {
        case Parsed.Success(result, _) => Success(result)
        case failure: Parsed.Failure => Failure(new RuntimeException(failure.trace().longMsg))
      }
    }
  }

  object input {
    import domain._
    import parser._

    def readRules(filePath: String): Map[Int, Rule] = {
      val strings = readElemsNewLine(filePath).toList
      val rs: List[Rule] = strings.map(parseRuleLine(_).get)
      rs.foldLeft(Map.empty[Int, Rule]){ case (m, r) => m.updated(r.id, r)}
    }

    def part2Update(rules: Map[Int, Rule]): Map[Int, Rule] = {
      rules
        .updated(8, XorRule(8, List(
          List(42),
          List(42, 8))))
        .updated(11, XorRule(11, List(
          List(42, 31),
          List(42, 11, 31))))
    }

    def readMessages(filePath: String): List[String] = {
      readElemsNewLine(filePath).toList
    }
  }

  object part1 {

    import domain._

    def rulify(msg: String, ruleId: Int, rules: Map[Int, Rule]): List[String] = {
      rules(ruleId) match {
        case FixedRule(_, prefix) =>
          if (msg startsWith prefix) List(msg.drop(prefix.length))
          else List(msg)

        case AndRule(_, subRules) =>
          subRules
            .foldLeft(List(msg)) { case (suffixes, subRuleId) =>
              suffixes.flatMap(suf => rulify(suf, subRuleId, rules))
            }
            .distinct

        case XorRule(_, subRulesLists) =>
          subRulesLists
            .flatMap {
              subRulesList =>
                subRulesList
                  .foldLeft(List(msg)) { case (suffixes, subRuleId) =>
                    suffixes.flatMap(suf => rulify(suf, subRuleId, rules))
                  }
                  .distinct
            }
      }
    }

    def conforms(as: List[String]): Boolean = as contains ""

    def solution(rules: Map[Int, Rule], msgs: List[String]): Int = {
      msgs.map { msg =>
        if (rulify(msg, 0, rules) contains "") 1 else 0
      }.sum
    }
  }

  object part2 {

  }

}

object Day19App extends App {
  import Day19._

  val rulesPath = "src/main/resources/input/day19_rules.in"
  val msgsPath = "src/main/resources/input/day19_messages.in"
  val testRulesPath = "src/main/resources/input/day19_rules.test"
  val testMsgsPath = "src/main/resources/input/day19_messages.test"

  val rules = input.readRules(rulesPath)
  val msgs = input.readMessages(msgsPath)
  val testRules = input.readRules(testRulesPath)
  val testMsgs = input.readMessages(testMsgsPath)

  val testSol1 = part1.solution(testRules, testMsgs)
  println(s"Test solution for part 1 is $testSol1")

  val sol1 = part1.solution(rules, msgs)
  println(s"Solution for part 1 is $sol1.")

  /**
   * Part 2
   */

  val rules2 = input.part2Update(rules)
  val sol2 = part1.solution(rules2, msgs)
  println(s"Solution for part 2 is $sol2.")



}

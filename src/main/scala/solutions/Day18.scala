package solutions

import fastparse._
import SingleLineWhitespace._
import solutions.Day07.domain.Formula
import utils.utils.readElemsNewLine

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Day18 {

  object domain {
    sealed trait Operator
    case object Add extends Operator
    case object Mul extends Operator

    sealed trait Expr
    case class Number(value: Int) extends Expr
    case class BinOp(left: Expr, op: Operator, right: Expr) extends Expr

    def eval(expr: Expr): Long = expr match {
      case Number(value) => value.toLong
      case BinOp(left, op, right) => op match {
        case Add => eval(left) + eval(right)
        case Mul => eval(left) * eval(right)
      }
    }
  }

  object parser1 {
    import domain._

    def number[_: P]: P[Expr] = P( CharsWhileIn("0-9").! ).map(_.toInt) map (a => Number(a))

    def operator[_: P]: P[Operator] = P("*" | "+").! map {
      case "*" => Mul
      case "+" => Add
    }

    def ws[_: P]: P[Unit] = P(" ".rep(1))


    def op[_: P]: P[Expr] = P(expr ~ operator ~ expr) map {
      case (left, op, right) => BinOp(left, op, right)
    }

    def expr[_: P]: P[Expr] = P( "(" ~ line ~ ")" | number)

    def line[_: P]: P[Expr] = {
      P(expr ~ (operator ~ expr).rep) map {
        case (lhs, rights) => rights.foldLeft(lhs) {
          case (left, (op, right)) => BinOp(left, op, right)
        }
      }
    }

    def parse(line: String): Try[Expr] = {
      fastparse.parse(line, parser1.line(_)) match {
        case Parsed.Success(result, _) => Success(result)
        case failure: Parsed.Failure => Failure(new RuntimeException(failure.trace().longMsg))
      }
    }
  }

  object parser2 {
    import domain._

    def number[_: P]: P[Expr] = P( CharsWhileIn("0-9").! ).map(_.toInt) map (a => Number(a))

    def operator[_: P]: P[Operator] = P("*" | "+").! map {
      case "*" => Mul
      case "+" => Add
    }

    def op[_: P]: P[Expr] = P(expr ~ operator ~ expr) map {
      case (left, op, right) => BinOp(left, op, right)
    }

    def expr[_: P]: P[Expr] = P( "(" ~ line ~ ")" | number)

    def line[_: P]: P[Expr] = {

      @tailrec
      def go(ps: Seq[(Operator, Expr)], acc: Seq[(Operator, Expr)]): Seq[(Operator, Expr)] = {
        ps match {
          case Seq() => acc
          case Seq(p: (Operator, Expr), ps1 @ _*) =>
            p._1 match {
              case Mul => go(ps1, acc.appended(p))
              case Add =>
                val (lastOperator, lastExpr) = acc.last
                val newLastExpr = BinOp(lastExpr, Add, p._2)
                val newAcc = acc.dropRight(1).appended((lastOperator, newLastExpr))
                go(ps1, newAcc)
            }
        }
      }
      val res: P[(Expr, Seq[(Operator, Expr)])] = P(expr ~ (operator ~ expr).rep)
      val squeezed: P[(Expr, Seq[(Operator, Expr)])] = {
        res map {
          case (e0, Seq()) => (e0, Seq())
          case (e0, Seq(p: (Operator, Expr), ps1 @ _*)) => (e0, go(ps1, Seq(p)))
        }
      }

      squeezed map {
        case (lhs, rights) => rights.foldLeft(lhs) {
          case (left, (op, right)) => BinOp(left, op, right)
        }
      }
    }

    def parse(line: String): Try[Expr] = {
      fastparse.parse(line, parser2.line(_)) match {
        case Parsed.Success(result, _) => Success(result)
        case failure: Parsed.Failure => Failure(new RuntimeException(failure.trace().longMsg))
      }
    }

  }

  object input {
    import parser1._
    import domain._

    def readInput1(filePath: String): List[Expr] = {
      val strings = readElemsNewLine(filePath).toList
      strings.map(parser1.parse(_).get)
    }

    def readInput2(filePath: String): List[Expr] = {
      val strings = readElemsNewLine(filePath).toList
      strings.map(parser2.parse(_).get)
    }
  }

  object part1 {
    import domain._

    def solution(exprs: List[Expr]): Long = {
      exprs.map(e => eval(e)).sum
    }
  }

}

object Day18App extends App {
  import Day18._
  import domain._

  val t1 = "1 + 2 * 3 + 4 * 5 + 6"
  val t2 = "1 + (2 * 3) + (4 * (5 + 6))"
  val t3 = "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
  val t4 = "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"



  val s1 = "1 + 2"
  val s2 = "1 + 2 * 3"
  val s3 = "1 + (2 * 3)"
  val s4 = "1 + 2 * 3"
  val s5 = "1 * 2 + 3"
  val s6 = "(2 * (8 + 5 + 5 + 7) + (2 + 9 + 8) * 8 * 8 * (4 * 8 * 7)) * ((6 + 2 + 7 * 2) * (4 * 2 + 9) + (4 + 3 + 8 + 8) + 4 + (2 * 4 * 9 * 9 + 4 + 6)) + 4 * (6 + 3 + (6 + 5 * 6 + 9)) + 9 + 7"

  println(s"$s1 --> ${parser2.parse(s1)}")
  println(s"$s2 --> ${parser2.parse(s2)}")
  println(s"$s3 --> ${parser2.parse(s3)}")
  println(s"$s4 --> ${parser2.parse(s4)}")
  println(s"$s5 --> ${parser2.parse(s5)}")

//  val ps1 = parseLine(s1)
//  val ps2 = parseLine(s2)
//  val ps3 = parseLine(s3)
//  val ps4 = parseLine(s4)
//
//  println(s"$s1 --> $ps1 --> ${eval(ps1.get)}")
//  println(s"$s2 --> $ps2 --> ${eval(ps2.get)}")
//  println(s"$s3 --> $ps3 --> ${eval(ps3.get)}")
//  println(s"$s4 --> $ps4 --> ${eval(ps4.get)}")

  val dataPath = "src/main/resources/input/day18.in"
  val exprs = input.readInput2(dataPath)
  val sol2 = part1.solution(exprs)
  println(s"Solution for part 2 is $sol2.")

}

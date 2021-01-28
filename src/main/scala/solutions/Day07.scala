package solutions

import fastparse._
import SingleLineWhitespace._
import utils.utils.readElemsNewLine

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

object Day07 {

  object domain {
    case class Bag(name: String)
    object Bag {
      def apply(adj: String, noun: String): Bag = Bag(s"$adj $noun")
    }

    case class Formula(bag: Bag, ingredients: List[(Bag, Int)])

    object Formula {

      object Parser {
        def number[_: P]: P[Int] = P(CharsWhileIn("0-9").!).map(_.toInt)

        def word[_: P]: P[String] = P(CharsWhileIn("a-z").!)

        def ingredient[_: P]: P[(Bag, Int)] =
          P(number ~ word ~ word ~ "bag" ~~ "s".?)
            .map {
              case (count, adj, noun) => (Bag(adj, noun), count)
            }

        def formula[_: P]: P[Formula] =
          P(word ~ word ~ "bags" ~ "contain" ~ ingredient.rep(sep = ",") ~ "." ~ End)
            .map {
              case (adj, noun, ingredients) => Formula(Bag(adj, noun), ingredients.toList)
            }
      }

      def parse(formula: String): Try[Formula] = {
        fastparse.parse(formula, Parser.formula(_)) match {
          case Parsed.Success(result, _) => Success(result)
          case failure: Parsed.Failure => Failure(new RuntimeException(failure.trace().longMsg))
        }
      }
    }

    sealed trait Status
    final case object NotVisited extends Status
    final case class Visited(count: Long) extends Status

    sealed trait VisitStatus
    final case object NotYetVisited extends VisitStatus
    final case object AlreadyVisited extends VisitStatus

    type StatusMap = Map[Bag, Status]
    type Graph = Map[Bag, List[(Bag, Int)]]

    type SimpleGraph = Map[Bag, List[Bag]]
    type VisitStatusMap = Map[Bag, VisitStatus]

    val target: Bag = Bag("shiny gold")

  }
  object parser {
    import domain._

    def parse(sentence: String): Try[Formula] = {
      val noOtherBagsRegex: Regex = """(\w+ \w+) bags contain no other bags.""".r
      sentence match {
        case noOtherBagsRegex(color) => Success(Formula(Bag(color), Nil))
        case otherwise => Formula.parse(sentence)
      }
    }
  }

  object input {
    import parser._
    import domain._
    def readInput(filePath: String): Graph = {
      val strings = readElemsNewLine(filePath) toList

      val formulas: List[Formula] = strings map parser.parse map {_.get}
      val graph = formulas map {f => f.bag -> f.ingredients} toMap

      graph
    }

    def reverseToSimpleGraph(graph: Graph): SimpleGraph = {

      @tailrec
      def go(bags: List[Bag], simpleGraph: SimpleGraph): SimpleGraph = {

        def traverse(bg: Bag, sg: SimpleGraph): SimpleGraph = {
          val neighbours: List[(Bag, Int)] = graph(bg)

          neighbours.foldLeft[SimpleGraph](sg) {
            case (g, (b, s)) =>
              g.get(b) match {
                case None => g.updated(b, List(bg))
                case Some(ns) => g.updated(b, bg :: ns)
              }
          }
        }

        bags match {
          case b :: bs => go(bs, traverse(b, simpleGraph))
          case Nil => simpleGraph
        }
      }

      val bags = graph.keys.toList
      val initGraph: SimpleGraph = bags.map{b => (b, Nil)}.toMap
      go(bags, initGraph)
    }
  }

  object part1 {
    import domain._



//    def solution(graph: Graph): Long = {
//
//      @tailrec
//      def go(bs: List[Bag], gr: Graph): Graph = {
//
//        def traverse(b: Bag, g: Graph): Graph = {
//          println(s"-- TRAVERSING bag [$b]")
//          g(b) match {
//            case (bis, NotVisited) =>
//              println(s"\t\t [$b]: not visited")
//              bis match {
//                case Nil => g.updated(b, (Nil, NotConnected))
//                case bi :: bis1 =>
//                  if (bi._1 == target) {
//                    g.updated(b, (bis1, Connected))
//                  }
//                  else {
//                    val updatedGraph = traverse(bi._1, g)
//                    val updatedStatus = updatedGraph(b)._1.foldLeft[Status](NotConnected) {
//                      case (status, (bag, _)) =>
//                        if (status == Connected || updatedGraph(bag)._2 == Connected)
//                          Connected
//                        else NotConnected
//                    }
//                    updatedGraph.updated(b, (bis1, updatedStatus))
//                  }
//              }
//            case _ => g
//          }
//        }
//
//        bs match {
//          case Nil => gr
//          case b :: bs1 => go(bs1, traverse(b, gr))
//        }
//      }
//
//      val vertices = graph.keys.toList
//      val traversedGraph = go(vertices, graph)
//      val ls: List[(Bag, (List[(Bag, Int)], Status))] = traversedGraph.toList
//
//      ls.foldLeft[Int](0){case (acc, (_, (_, status))) => if (status == Connected) acc + 1 else acc}
//    }

    def solutionWithReversedGraph(graph: SimpleGraph): Long = {

      def go(bag: Bag, statusMap: VisitStatusMap): (Long, VisitStatusMap) = {
        val neighbours = graph(bag)
        val updatedStatusMap = statusMap.updated(bag, AlreadyVisited)
        val (resCount, resStatus) = neighbours.foldLeft[(Long, VisitStatusMap)]((0, updatedStatusMap)) {
          case ((acc: Long, status: VisitStatusMap), b: Bag) =>
            val (a: Long, stat: VisitStatusMap) =
              status(b) match {
                case AlreadyVisited =>
                  (0L, status)
                case NotYetVisited =>
                  go(b, status)
                case oth => throw new Exception(s"OTHER: $oth")
              }
            (acc + a, stat)
        }
        (resCount + 1L, resStatus)
      }

      val statusMap: VisitStatusMap = graph.keys.toList.map(b => (b, NotYetVisited)).toMap
      val (res, _) = go(target, statusMap)
      res
    }
  }

  object part2 {
    import domain._

    def solution(graph: Graph): Long = {

      def go(bag: Bag, multiplier: Int, statusMap: StatusMap): (Long, StatusMap) = {
        val neighbours: List[(Bag, Int)] = graph(bag)
        val (subGraphCount, subGraphStatus) =
          neighbours.foldLeft[(Long, StatusMap)]((0, statusMap)) {
            case ((acc: Long, status: StatusMap), (b, i)) =>
              val (a, stat) =
                status(b) match {
                  case NotVisited =>
                    go(b, i, status)
                  case Visited(count) =>
                    (count, status)
                }
              (acc + a, stat)
          }
        (multiplier * (subGraphCount + 1), subGraphStatus)
      }

      val statusMap: StatusMap = graph.keys.toList.map(b => (b, NotVisited)).toMap
      go(target, 1, statusMap)._1 - 1
    }
  }

}

object Day07App extends App {
  import Day07._

  val dataPath = "src/main/resources/input/day07.in"

  val testPath = "src/main/resources/input/day07.test"
  val testGraph = input.readInput(testPath)
  println("--- graph")
  testGraph.foreach(println)
  val testSol2 = part2.solution(testGraph)
  println(s"Solution for test in part 2 is $testSol2.")


//  val testRevGraph = input.reverseToSimpleGraph(testGraph)
//  println("-- rev graph")
//  testRevGraph.foreach(println)
//  val testSol = part1.solutionWithReversedGraph(testRevGraph)
//  println(s"-- test sol = $testSol")


  val inputGraph = input.readInput((dataPath))
//  val inputRevGraph = input.reverseToSimpleGraph(inputGraph)
//  val sol1 = part1.solutionWithReversedGraph(inputRevGraph)
//  println(s"Part 1 sol is $sol1.")
  val sol2 = part2.solution(inputGraph)
  println(s"Solution for part 2 is $sol2.")

//  val sol1 = part1.solution(inputGraph)
//  println(sol1)

//  val test2Graph = input.readInput(test2Path)
//  test2Graph.foreach(println)
//  val rev2 = input.reverseToSimpleGraph(test2Graph)
//  println(s"-- REVED:")
//  rev2.foreach(println)


//  val test2Sol = part1.solution(test2Graph)
//  println(test2Sol)
//
//  val test3Graph = input.readInput(test3Path)
//  val test3Sol = part1.solution(test3Graph)
//  println(test3Sol)
//
//  val test4Graph = input.readInput(test4Path)
//  val test4Sol = part1.solution(test4Graph)
//  println(test4Sol)

  val test2Path = "src/main/resources/input/day07_2.test"
  val test3Path = "src/main/resources/input/day07_3.test"
  val test4Path = "src/main/resources/input/day07_4.test"
}

package solutions

import fastparse._
import SingleLineWhitespace._
import utils.utils.readElemsNewLine

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Day21 {

  object domain {
    case class Ingredient(name: String)
    case class Allergen(name: String)
    case class Food(ingredients: Set[Ingredient], allergens: List[Allergen])
  }

  object parser {
    import domain._

    def word[_: P]: P[String] = P(CharsWhileIn("a-z").!)

    def line[_: P] =
      P(word.rep() ~ "(contains " ~ word.rep(sep = ", ") ~ ")" ~ End)
        .map {
          case (is, as) =>
            Food(is.toSet.map(i => Ingredient(i)), as.toList.map(a => Allergen(a)))
        }

    def parseLine(str: String) = {
      fastparse.parse(str, line(_)) match {
        case Parsed.Success(result, _) => Success(result)
        case failure: Parsed.Failure => Failure(new RuntimeException(failure.trace().longMsg))
      }
    }
  }

  object input {
    import domain._
    import parser._

    def readInput(filepath: String) = {
      val strings = readElemsNewLine(filepath)
      strings.map(parseLine(_).get).toList
    }


  }

  object part1 {
    import domain._

    type All2IngMap = Map[Allergen, Set[Ingredient]]
    type All2IngList = List[(Allergen, Set[Ingredient])]
    def allergensMap(foods: List[Food]): All2IngMap = {

      def processFood(f: Food, mapAcc: All2IngMap): All2IngMap = {
        def go(as: List[Allergen], m: All2IngMap): All2IngMap = as match {
          case Nil => m
          case a :: as1 =>
            if (m contains a) {
              val intersection = m(a) intersect f.ingredients
              go(as1, m.updated(a, intersection))
            }
            else
              go(as1, m.updated(a, f.ingredients))
        }
        go(f.allergens, mapAcc)
      }

      @tailrec
      def go(fs: List[Food], mapAcc: All2IngMap): All2IngMap = fs match {
        case f :: fs1 => go(fs1, processFood(f, mapAcc))
        case Nil => mapAcc
      }
      go(foods, Map.empty)
    }

    def assign(all2Ings: List[(Allergen, Set[Ingredient])]): List[(Allergen, Ingredient)] = {

      @tailrec
      def go(as: All2IngList, acc: List[(Allergen, Ingredient)]): List[(Allergen, Ingredient)] =
        as match {
          case Nil => acc
          case a :: as1 =>
            if (a._2.size == 1) {
              val ing = a._2.head
              val rest =
                as1
                  .map { case (a, is) => (a, is - ing)}
                  .sortBy(_._2.size)
              go(rest, (a._1, ing) :: acc)
            }
            else {
              throw new RuntimeException(s"Unable to assign for $a.")
            }
        }

      go(all2Ings, Nil)
    }

    def solution(foods: List[Food]) = {
      val m = part1.allergensMap(foods)
      val l: List[(Allergen, Set[Ingredient])] = m.toList.sortBy(_._2.size)
      val assignedPairs: List[(Allergen, Ingredient)] = part1.assign(l)
      val assignedIngs: List[Ingredient] = assignedPairs.map { case (_, i) => i}

      val c = {
        foods.foldLeft(0) {
          case (acc, f) =>
            val count = {
              f.ingredients.foldLeft(0) {
                case (a, ing) =>
                  if (assignedIngs contains ing) a
                  else a + 1
              }
            }
            acc + count
        }

      }
      (c, assignedPairs)
    }
  }
}

object Day21App extends App {
  import Day21._
  import domain._
  val testPath = "src/main/resources/input/day21.test"
  val inPath = "src/main/resources/input/day21.in"

  val inFoods: List[Food] = input.readInput(inPath)
  val sol1 = part1.solution(inFoods)
  println(sol1)
  val ps = sol1._2
  val sorted = ps.sortBy { case (a, i) => a.name}
  println(s"SORTED= $sorted")
  val sol2 = sorted.map { case (a, i) => i.name + ","}.mkString
  println(sol2)





}

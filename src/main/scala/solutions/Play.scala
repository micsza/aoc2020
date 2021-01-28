package solutions

import scala.util.matching.Regex

object Play extends App {
  val sentence = "white phial potion is 3 fortify health potions, 1 vigorous stamina potion, 5 plentiful magicka potions."
  val recipePattern: Regex = """(\w+ \w+) potion is( (\d+) (\w+ \w+) (?:potion|potions)[,.])+""".r
  val recipePattern2: Regex = """(\w+ \w+) potion is([^,.])""".r

  case class Potion(name: String)
  case class Formula(potion: Potion, ingredients: Option[(Potion, Int)])

  sentence match {
    case recipePattern2(potion, group) => println(s"[$sentence]: matched [$potion]")
    case _ => println(s"[$sentence]: fail.")
  }
  println("^^^^^^^^^^^^^^^^^^^^^^")

  val bagReg: Regex = """(\w+ \w+) bags contain(.+)""".r
  val sentence1 = "striped violet bags contain 2 clear violet bags, 1 striped yellow bag, 5 dark lime bags."
  sentence1 match {
    case bagReg(gr1, gr2) => println(s"--matched: [$gr1] [$gr2]")
    case _ => println("fail")
  }

  val sentence2 = " 2 clear violet bags, 1 striped yellow bag, 5 dark lime bags."
  val sepPattern: Regex = """([^,.])+""".r
  for {
    res <- sepPattern findAllIn sentence2
  } println(res)

  def parseFormula(sentence: String) = {
    val baseReg: Regex = """(\w+ \w+) bags contain(.+)""".r
    val sepPattern: Regex = """([^,.])+""".r
    val sub: Regex = """ (\d+) (\w+ \w+) (?:bags|bag)[,.]""".r

    sentence match {
      case baseReg(base, rest) =>
        println(s"base ok: base = [$base], rest = [$rest].")
        val yields = for(elem <- sepPattern findAllIn rest) yield elem


        match {
          case sub(num, color) => println(s"SUB: [$num] [$color]")
          case _ => println("huj fail")
        }

      case _ => println("base fail")
    }
  }

  println("&&&&&&&&&&&&&&&&&&7")
  parseFormula(sentence1)




}

package solutions

import utils.utils._

import scala.util.matching.Regex

object Day04 {

  object domain {

    type Pair = (String, String)
    type Document = List[ExtendedField]
    type ExtendedField = (FieldType, Field)

    sealed trait FieldType
    case object BirthYearField extends FieldType
    case object IssueYearField extends FieldType
    case object ExpirationYearField extends FieldType
    case object HeightField extends FieldType
    case object HairColorField extends FieldType
    case object EyeColorField extends FieldType
    case object PassportIdField extends FieldType
    case object CountryIdField extends FieldType

    sealed trait Field
    case class BirthYear(year: String) extends Field
    case class IssueYear(year: String) extends Field
    case class ExpirationYear(year: String) extends Field
    case class Height(height: String) extends Field
    case class HairColor(color: String) extends Field
    case class EyeColor(color: String) extends Field
    case class PassportId(id: String) extends Field
    case class CountryId(id: String) extends Field

    val requiredFields: List[FieldType] = List(
      BirthYearField,
      IssueYearField,
      ExpirationYearField,
      HeightField,
      HairColorField,
      EyeColorField,
      PassportIdField
    )
  }
  object input {
    import domain._

    def readStringReprs(filePath: String): List[List[String]] = {
      val oneString = readFileToString(filePath)
      val split = oneString.split("\\n\\n").toList.map(_.split("\\s").toList)
      split
    }

    def parsePair(str: String): Pair = {
      val pattern: Regex = "([A-Za-z]{3}):(\\S+)".r
      val pattern(key, value) = str
      (key, value)
    }

    def parseField(pair: Pair): ExtendedField = pair._1 match {
      case "byr" => (BirthYearField, BirthYear(pair._2))
      case "iyr" => (IssueYearField, IssueYear(pair._2))
      case "eyr" => (ExpirationYearField, ExpirationYear(pair._2))
      case "hgt" => (HeightField, Height(pair._2))
      case "hcl" => (HairColorField, HairColor(pair._2))
      case "ecl" => (EyeColorField, EyeColor(pair._2))
      case "pid" => (PassportIdField, PassportId(pair._2))
      case "cid" => (CountryIdField, CountryId(pair._2))
    }

    def parseFields(stringRepr: List[String]): Document = {
      val pairs: List[Pair] = stringRepr.map(parsePair)
      val fields: List[ExtendedField] = pairs.map(parseField)
      fields
    }

    def readDocuments(filePath: String): List[Document] = {
      val reprs: List[List[String]] = readStringReprs(filePath)
      reprs.map(parseFields)
    }
  }

  object part1 {
    import domain._
    def validateDocument(document: Document): Boolean = {
      requiredFields.forall(requiredField => {
        val xs = document.flatMap(p => List(p._1))
        xs.contains(requiredField)
      })
    }

    def solution(documents: List[Document]): Long = {
      def go(ds: List[Document], counter: Long): Long = ds match {
        case Nil => counter
        case d :: ds1 => {
          val a = if (validateDocument(d)) 1 else 0
          go(ds1, counter + a)
        }
      }

      go(documents, 0L)
    }
  }

  object part2 {
    import domain._

    def validateDocumentForRequiredFields(document: Document): Boolean = {
      val ret = requiredFields.forall(requiredField => {
        val xs = document.flatMap(p => List(p._1))
        xs.contains(requiredField)
      })
      ret
    }

    def validateField(field: Field): Boolean = {
      val fourDigitRegex: Regex = "[0-9]{4}".r
      val numRegex = "(\\d+)".r
      val CmHeightPattern: Regex = "(\\d+)cm".r
      val InHeightPattern: Regex = "(\\d+)in".r
      val HairColorPattern: Regex = "#([0-9a-f]{6})".r
      val EyeColorPattern: Regex = "(amb|blu|brn|gry|grn|hzl|oth)".r
      val PassportIdPattern: Regex = "([\\d]{9})".r

      field match {
        case BirthYear(byr) => fourDigitRegex.matches(byr) && (byr.toInt >= 1920) && (byr.toInt <= 2002)
        case IssueYear(iyr) => fourDigitRegex.matches(iyr) && (iyr.toInt >= 2010) && (iyr.toInt <= 2020)
        case ExpirationYear(yr) => fourDigitRegex.matches(yr) && (yr.toInt >= 2020) && (yr.toInt <= 2030)
        case Height(height) => height match {
          case CmHeightPattern(h) => (h.toInt >= 150) && (h.toInt <= 193)
          case InHeightPattern(h) => (h.toInt >= 59) && (h.toInt <= 76)
          case _ => false
        }
        case HairColor(color) => color match {
          case HairColorPattern(_) => true
          case _ => false
        }
        case EyeColor(color) => color match {
          case EyeColorPattern(_) => true
          case _ => false
        }
        case PassportId(id) => id match {
          case PassportIdPattern(_) => true
          case _ => false
        }
        case CountryId(_) => true
        case _ => false
      }
    }

    def validateDocumentForParamRequirements(document: Document): Boolean =
      document.forall(extendedField => validateField(extendedField._2))

    def validateDocument(document: Document): Boolean =
      validateDocumentForRequiredFields(document) && validateDocumentForParamRequirements(document)

    def solution(documents: List[Document]): Long = {
      def go(ds: List[Document], counter: Long): Long = ds match {
        case Nil => counter
        case d :: ds1 => {
          val a = if (validateDocument(d)) 1 else 0
          go(ds1, counter + a)
        }
      }
      go(documents, 0L)
    }
  }
}

object Day04App extends App {
  import Day04._
  import Day04.domain._
  val dataPath = "src/main/resources/input/day04.in"
  val testDataPath = "src/main/resources/input/day04.test"

  val ds = input.readDocuments(dataPath)
  val sol1 = part1.solution(ds)
  val sol2 = part2.solution(ds)
  println(s"Solution for part 1 is $sol1.\nSolution for part 2 is $sol2.")

}

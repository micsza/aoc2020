package solutions

import utils.utils.readElemsNewLine

import scala.annotation.tailrec
import scala.util.matching.Regex

case class Record(lo: Int, hi: Int, letter: Char, password: String)

object input {
  val recordPattern: Regex = "([0-9]+)-([0-9]+) ([A-Za-z]): ([A-Za-z]+)".r

  def parseRecord(str: String, pattern: Regex): Record = {
    val pattern(m, n, c, cs) = str
    Record(m.toInt, n.toInt, c.head, cs)
  }

  def readRecords(as: Seq[String]): Seq[Record] =
    as.map(s => parseRecord(s, recordPattern))

  val dataPath = "src/main/resources/input/day2.in"
  val as: Seq[String] = readElemsNewLine(dataPath)
  val records: List[Record] = readRecords(as).toList
}

object Day2Part1 {

  def validRecord(record: Record): Int = {
    val count = record.password.count(_ == record.letter)
    if (record.lo <= count && count <= record.hi) 1 else 0
  }

  def solution(records: List[Record], validFunc: Record => Int): Int = {
    @tailrec
    def go(rs: List[Record], counter: Int): Int = rs match {
      case Nil => counter
      case r :: rs1 => go(rs1, counter + validFunc(r))
    }

    go(records, 0)
  }
}

object Day2Part2 {

  def validRecord(record: Record): Int = {
    val valid: Boolean = (record.password(record.lo - 1) == record.letter)  != (record.password(record.hi - 1) == record.letter)
    if (valid) 1 else 0
  }
}

object Day2 extends App {
  val sol1 = Day2Part1.solution(input.records, Day2Part1.validRecord)
  val sol2 = Day2Part1.solution(input.records, Day2Part2.validRecord)
  println(s"Solution for part 1 is $sol1.\nSolution for part 2 is $sol2.")
}

package utils

object utils {
  def readElemsNewLine(filename: String): Seq[String] = {
    val bufferedSource = io.Source.fromFile(filename)
    val lines = (for (line <- bufferedSource.getLines()) yield line).toSeq
    bufferedSource.close
    lines
  }

  def readElemsSeparator(filename: String, sep: String) = {
    val bufSource = scala.io.Source.fromFile(filename)
    val buf = scala.collection.mutable.ListBuffer.empty[String]
    for (line <- bufSource.getLines) {
      val values = line.split(sep).map(_.trim).toSeq
      buf.appendAll(values)
    }
    bufSource.close
    buf
  }



  def readIntElemsSeparator(filename: String, sep: String) = {
    val bufSource = scala.io.Source.fromFile(filename)
    val buf = scala.collection.mutable.ListBuffer.empty[Int]
    for (line <- bufSource.getLines) {
      val values = line.split(sep).map(_.trim).map(_.toInt).toSeq
      buf.appendAll(values)
    }
    bufSource.close
    buf
  }

  def readFileToString(filePath: String): String = {
    val source = scala.io.Source.fromFile(filePath)
    val lines = try source.mkString finally source.close
    lines
  }

  def readEmptyNewLineSeparatedStrings(filePath: String): List[String] = {
    val oneString = readFileToString(filePath)
    val split = oneString.split("\\n\\n").toList
    split
  }



}

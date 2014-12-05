package hu.matan.log.parser

import scala.collection.immutable.List
import scala.util.parsing.combinator._

object Log4JParser extends JavaTokenParsers {

  def log4JLine = logLine | exceptionLine | stackTraceLine

  def logLine = channel ~ time ~ "," ~ category ~ "," ~ ".*".r ^^ {
    case ch ~ ti ~ comma1 ~ ca ~ comma2 ~ rest => LogLine(ch, ti, ca, rest)
  }

  def exceptionLine = channel ~ exceptionClass ~ ": " ~ ".*".r ^^ {
    case ch ~ ec ~ colon ~ msg => ExceptionLine(ch, ec, msg)
  }

  //  "        [java] \tat org.springframework.jdbc.datasource.DataSourceUtils.getConnection(DataSourceUtils.java:80) ~[org.springframework.jdbc_3.0.5.RELEASE.jar:3.0.5.RELEASE]"
  def stackTraceLine = channel ~ "at" ~ pcm ~ "(" ~ file ~ ":" ~ line ~ ")" ~ opt(jar) ^^ {
    case ch ~ at ~ pcm ~ openBrace ~ file ~ colon ~ line ~ closeBrace ~ jar => StackTraceLine(ch, pcm.`package`, pcm.`class`, pcm.`method`, file, line, jar)
  }


  def pcm: Parser[Pcm] = repsep( """\w+""".r, ".") ^^ {
    case list: List[String] => Pcm(
      `package` = list.dropRight(2).mkString("."),
      `class` = list.reverse.tail.head,
      `method` = list.reverse.head
    )
  }

  def file: Parser[String] = """\w+\.?\w*""".r

  def line: Parser[Long] = """\d+""".r ^^ {
    case num => num.toLong
  }

  def jar: Parser[String] = "~" ~> "[" ~> """[\w\._\:]+""".r <~ "]"


  def exceptionClass: Parser[String] = """[\w\.]+""".r

  def channel: Parser[String] = "[" ~> "\\w+".r <~ "]"

  def time: Parser[String] = "[\\w:]+".r

  def category: Parser[String] = "\\w+".r


  def parse(source: String): Log4JLine = parseAll(log4JLine, source) match {
    case Success(expression, _) => expression
    case f: NoSuccess => throw new IllegalArgumentException(f.msg)
  }
}

case class Pcm(`package`: String, `class`: String, `method`: String)

sealed trait Log4JLine

case class LogLine(channel: String, time: String, category: String, rest: String) extends Log4JLine

case class ExceptionLine(channel: String, exceptionClass: String, message: String) extends Log4JLine


//StackTraceLine("java", "org.springframework.jdbc.datasource.DataSourceUtils.getConnection(DataSourceUtils.java", 80, Option("org.springframework.jdbc_3.0.5.RELEASE.jar:3.0.5.RELEASE"))

case class StackTraceLine(channel: String, `package`: String, `class`: String, `method`: String, file: String, line: Long, jar: Option[String]) extends Log4JLine
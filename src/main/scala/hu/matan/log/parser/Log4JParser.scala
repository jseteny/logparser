package hu.matan.log.parser

import scala.collection.immutable.List
import scala.util.parsing.combinator._

/*
http://www.donroby.com/wp/scala/parsing-expressions-with-scala-parser-combinators-2/
 */

object Log4JParser extends RegexParsers {

  override protected val whiteSpace = """[ \t]+""".r

  def fileContent = repsep(logLine | exceptionLine | stackTraceLine | commonFramesOmitted, endOfLine)

  def endOfLine = "\r\n" | "\n"

  def logLine = channel ~ time ~ "," ~ category ~ "," ~ ".*".r ^^ {
    case ch ~ ti ~ comma1 ~ ca ~ comma2 ~ rest => LogLine(ch, ti, ca, rest)
  }

  /**
   * Example input:
   * <pre>
   * "        [java] Caused by: com.mysql.jdbc.exceptions.jdbc4.MySQLNonTransientConnectionException: Could not create connection to database server. Attempted reconnect 3 times. Giving up."
   * "        [java]            org.springframework.jdbc.CannotGetJdbcConnectionException: Could not get JDBC Connection; nested exception is com.mysql.jdbc.exceptions.jdbc4.MySQLNonTransientConnectionException: Could not create connection to database server. Attempted reconnect 3 times. Giving up."
   * </pre>
   */
  def exceptionLine = channel ~ opt("Caused" ~ "by:") ~ exceptionClass ~ ": " ~ ".*".r ^^ {
    case ch ~ causedBy ~ ec ~ colon ~ msg => ExceptionLine(ch, ec, msg, causedBy!= None)
  }

  /**
   * Example input:
      <pre>
      "        [java] \tat org.springframework.jdbc.datasource.DataSourceUtils.getConnection(DataSourceUtils.java:80) ~[org.springframework.jdbc_3.0.5.RELEASE.jar:3.0.5.RELEASE]"
      </pre>
   * and the output that belongs to the above input:
      <pre>
      StackTraceLine(
          channel = "java",
          `package` = "org.springframework.jdbc.datasource",
          `class` = "DataSourceUtils",
          `method` = "getConnection",
          file = "DataSourceUtils.java",
          line = 80,
          jar = Some("org.springframework.jdbc_3.0.5.RELEASE.jar:3.0.5.RELEASE")
        )
      </pre>
   */
  def stackTraceLine = channel ~ "at" ~ pcm ~ "(" ~ file ~ ":" ~ line ~ ")" ~ opt(jar) ^^ {
    case ch ~ at ~ pcm ~ openBrace ~ file ~ colon ~ line ~ closeBrace ~ jar
    => StackTraceLine(ch, pcm.`package`, pcm.`class`, pcm.`method`, file, line, jar)
  }


  /**
   * Example input: "        [java] \t... 32 common frames omitted"
   */
  def commonFramesOmitted = channel ~ "..." ~ """\d+""".r <~ "common frames omitted" ^^{
    case ch ~ dots ~ num => CommonFramesOmitted(num.toInt)
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
    case num: String => num.toLong
  }

  def jar: Parser[String] = "~" ~> "[" ~> """[\w\._\:]+""".r <~ "]"


  def exceptionClass: Parser[String] = """[\w\.]+""".r

  def channel: Parser[String] = "[" ~> "\\w+".r <~ "]"

  def time: Parser[String] = "[\\w:]+".r
  
  def category: Parser[String] = "\\w+".r


  def parse(source: String): List[Log4JLine] = parseAll(fileContent, source) match {
    case Success(expression, _) => expression
    case f: NoSuccess => throw new IllegalArgumentException(f.msg)
  }

  def parse(source: java.io.Reader): List[Log4JLine] = parseAll(fileContent, source) match {
    case Success(expression, _) => expression
    case f: NoSuccess => throw new IllegalArgumentException(f.msg)
  }
}


case class Pcm(`package`: String, `class`: String, `method`: String)


sealed trait Log4JLine

case class LogLine(channel: String, time: String, category: String, rest: String) extends Log4JLine

case class ExceptionLine(channel: String, exceptionClass: String, message: String, isCause: Boolean) extends Log4JLine

case class StackTraceLine(channel: String, `package`: String, `class`: String, `method`: String, file: String, line: Long, jar: Option[String]) extends Log4JLine

case class CommonFramesOmitted(number: Int) extends Log4JLine
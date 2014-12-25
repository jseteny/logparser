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
    case ch ~ causedBy ~ ec ~ colon ~ msg => ExceptionLine(ch, ec, msg, causedBy != None)
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
  def stackTraceLine = channel ~ "at" ~ pcmfl ~ opt(jar) ^^ {
    case ch ~ at ~ pcmfl ~ jar => StackTraceLine(ch, pcmfl, jar)
  }


  /**
   * Example input: "        [java] \t... 32 common frames omitted"
   */
  def commonFramesOmitted = channel ~ "..." ~ """\d+""".r <~ "common frames omitted" ^^ {
    case ch ~ dots ~ num => CommonFramesOmitted(num.toInt)
  }

  def pcmfl: Parser[Pcmfl] = repsep( """\w+""".r, ".") ~ (fileLine | fileOrNativeOrUnknown) ^^ {
    case list ~ (_fileLine: (String, Long)) => Pcmfl(
      `package` = list.dropRight(2).mkString("."),
      `class` = list.reverse.tail.head,
      method = list.reverse.head,
      file = Some(_fileLine._1),
      line = Some(_fileLine._2),
      isNative = false,
      isUnknownSource = false
    )
    case list ~ "(Native Method)" => Pcmfl(
      `package` = list.dropRight(2).mkString("."),
      `class` = list.reverse.tail.head,
      method = list.reverse.head,
      file = None,
      line = None,
      isNative = true,
      isUnknownSource = false
    )
    case list ~ "(Unknown Source)" => Pcmfl(
      `package` = list.dropRight(2).mkString("."),
      `class` = list.reverse.tail.head,
      method = list.reverse.head,
      file = None,
      line = None,
      isNative = false,
      isUnknownSource = true
    )
  }

  def fileOrNativeOrUnknown = "(Native Method)" | "(Unknown Source)"

  def fileLine: Parser[(String, Long)] = "(" ~> file ~ ":" ~ line <~ ")" ^^ {
    case f ~ colon ~ l => (f, l)
  }

  def file: Parser[String] = """\w+\.?\w*""".r

  def line: Parser[Long] = """\d+""".r ^^ {
    case num: String => num.toLong
  }

  def jar: Parser[String] = opt("~") ~> "[" ~> """[\w\._\-\:]+""".r <~ "]"


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
    case NoSuccess(err, next) => throw new IllegalArgumentException("failed to parse " +
      "(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
      err + "\n" + next.pos.longString)
  }

  def parseChunk[T](p: Parser[T], in: java.lang.CharSequence): T = parseAll(p, in) match {
    case Success(expression, _) => expression
    case f: NoSuccess => throw new IllegalArgumentException(f.msg)
  }

}


case class Pcmfl(`package`: String, `class`: String, method: String, file: Option[String], line: Option[Long], isNative: Boolean, isUnknownSource: Boolean)


sealed trait Log4JLine

case class LogLine(channel: String, time: String, category: String, rest: String) extends Log4JLine

case class ExceptionLine(channel: String, exceptionClass: String, message: String, isCause: Boolean) extends Log4JLine

case class StackTraceLine(channel: String, pcmfl: Pcmfl, jar: Option[String]) extends Log4JLine

case class CommonFramesOmitted(number: Int) extends Log4JLine

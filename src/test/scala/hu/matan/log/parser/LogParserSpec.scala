package hu.matan.log.parser

import java.io.Reader

import org.specs2.mutable._

import scala.io.Source

class LogParserSpec extends Specification {

  "        [java] 15:13:29, INFO,  , d.c.o.d.internal.DBAssistent - Processing global schema 'globalschema'." should (
    """be parsed as List(LogLine(
      |        channel = "java",
      |        time = "15:13:29",
      |        category = "INFO",
      |        rest = ", d.c.o.d.internal.DBAssistent - Processing global schema 'globalschema'."
      |      ))""".stripMargin in {

      val input = "        [java] 15:13:29, INFO,  , d.c.o.d.internal.DBAssistent - Processing global schema 'globalschema'."
      val result = Log4JParser.parse(input)

      result must be_===(List(LogLine(
        channel = "java",
        time = "15:13:29",
        category = "INFO",
        rest = ", d.c.o.d.internal.DBAssistent - Processing global schema 'globalschema'."
      )))
    })


  "        [java] org.springframework.jdbc.CannotGetJdbcConnectionException: Could not get JDBC Connection; nested exception is com.mysql.jdbc.exceptions.jdbc4.MySQLNonTransientConnectionException: Could not create connection to database server. Attempted reconnect 3 times. Giving up." should (
    """be parsed as List(ExceptionLine(
      |        channel = "java",
      |        exceptionClass = "org.springframework.jdbc.CannotGetJdbcConnectionException",
      |        message = "Could not get JDBC Connection; nested exception is com.mysql.jdbc.exceptions.jdbc4.MySQLNonTransientConnectionException: Could not create connection to database server. Attempted reconnect 3 times. Giving up."
      |      ))""".stripMargin in {

      val input = "        [java] org.springframework.jdbc.CannotGetJdbcConnectionException: Could not get JDBC Connection; nested exception is com.mysql.jdbc.exceptions.jdbc4.MySQLNonTransientConnectionException: Could not create connection to database server. Attempted reconnect 3 times. Giving up."
      val result = Log4JParser.parse(input)

      result must be_===(List(ExceptionLine(
        channel = "java",
        exceptionClass = "org.springframework.jdbc.CannotGetJdbcConnectionException",
        message = "Could not get JDBC Connection; nested exception is com.mysql.jdbc.exceptions.jdbc4.MySQLNonTransientConnectionException: Could not create connection to database server. Attempted reconnect 3 times. Giving up.",
        isCause = false
      )))
    })

  "        [java] Caused by: com.mysql.jdbc.exceptions.jdbc4.MySQLNonTransientConnectionException: Could not create connection to database server. Attempted reconnect 3 times. Giving up." should (
    "be parsed as" in {

      val input = "        [java] Caused by: com.mysql.jdbc.exceptions.jdbc4.MySQLNonTransientConnectionException: Could not create connection to database server. Attempted reconnect 3 times. Giving up."
      val result = Log4JParser.parse(input)

      result must be_===(List(ExceptionLine(
        channel = "java",
        exceptionClass = "com.mysql.jdbc.exceptions.jdbc4.MySQLNonTransientConnectionException",
        message = "Could not create connection to database server. Attempted reconnect 3 times. Giving up.",
        isCause = true
      )))
    })

  "        [java] \tat org.springframework.jdbc.datasource.DataSourceUtils.getConnection(DataSourceUtils.java:80) ~[org.springframework.jdbc_3.0.5.RELEASE.jar:3.0.5.RELEASE]" should (
    """be parsed as List(StackTraceLine(
      |          channel = "java",
      |          `package` = "org.springframework.jdbc.datasource",
      |          `class` = "DataSourceUtils",
      |          `method` = "getConnection",
      |          file = "DataSourceUtils.java",
      |          line = 80,
      |          jar = Some("org.springframework.jdbc_3.0.5.RELEASE.jar:3.0.5.RELEASE")
      |        ))""".stripMargin in {

      val input = "        [java] \tat org.springframework.jdbc.datasource.DataSourceUtils.getConnection(DataSourceUtils.java:80) ~[org.springframework.jdbc_3.0.5.RELEASE.jar:3.0.5.RELEASE]"
      val result = Log4JParser.parse(input)

      result must be_===(List(StackTraceLine(
        channel = "java",
        `package` = "org.springframework.jdbc.datasource",
        `class` = "DataSourceUtils",
        `method` = "getConnection",
        file = "DataSourceUtils.java",
        line = 80,
        jar = Some("org.springframework.jdbc_3.0.5.RELEASE.jar:3.0.5.RELEASE")
      )))
    })

  "        [java] Caused by: com.mysql.jdbc.exceptions.jdbc4.MySQLNonTransientConnectionException: Could not create connection to database server. Attempted reconnect 3 times. Giving up." +
    "        [java] \tat org.springframework.jdbc.datasource.DataSourceUtils.getConnection(DataSourceUtils.java:80) ~[org.springframework.jdbc_3.0.5.RELEASE.jar:3.0.5.RELEASE]" should (
    """be parsed as List(ExceptionLine, StackTraceLine)""" in {

      val input = "        [java] Caused by: com.mysql.jdbc.exceptions.jdbc4.MySQLNonTransientConnectionException: Could not create connection to database server. Attempted reconnect 3 times. Giving up.\n" +
        "        [java] \tat org.springframework.jdbc.datasource.DataSourceUtils.getConnection(DataSourceUtils.java:80) ~[org.springframework.jdbc_3.0.5.RELEASE.jar:3.0.5.RELEASE]"

      val result = Log4JParser.parse(input)

      result must be_===(List(
        ExceptionLine(
          channel = "java",
          exceptionClass = "com.mysql.jdbc.exceptions.jdbc4.MySQLNonTransientConnectionException",
          message = "Could not create connection to database server. Attempted reconnect 3 times. Giving up.",
          isCause = true
        ),
        StackTraceLine(
          channel = "java",
          `package` = "org.springframework.jdbc.datasource",
          `class` = "DataSourceUtils",
          `method` = "getConnection",
          file = "DataSourceUtils.java",
          line = 80,
          jar = Some("org.springframework.jdbc_3.0.5.RELEASE.jar:3.0.5.RELEASE")
        )
      ))
    })

  "        [java] \t... 32 common frames omitted" should (
    "be parsed as List(CommonFramesOmitted(32))" in {

      val input = "        [java] \t... 32 common frames omitted"

      val result = Log4JParser.parse(input)

      result must be_===(List(CommonFramesOmitted(32)))
    })

  """The "mixed log4j and ant.log" file""" should (
    "be parsed" in {

      val input = Source.fromFile("src/test/resources/truncated mixed log4j and ant.log").bufferedReader()

      val result = Log4JParser.parse(input)

      result.size must be_==(0)
    })
}

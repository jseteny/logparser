package hu.matan.log.parser

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
      |        channel = "java",
      |        Pcmfl(
      |          `package` = "org.springframework.jdbc.datasource",
      |          `class` = "DataSourceUtils",
      |          method = "getConnection",
      |          file = Some("DataSourceUtils.java"),
      |          line = Some(80),
      |          isNative = false,
      |          isUnknownSource = false
      |        ),
      |        jar = Some("org.springframework.jdbc_3.0.5.RELEASE.jar:3.0.5.RELEASE")
      |      ))""".stripMargin in {

      val input = "        [java] \tat org.springframework.jdbc.datasource.DataSourceUtils.getConnection(DataSourceUtils.java:80) ~[org.springframework.jdbc_3.0.5.RELEASE.jar:3.0.5.RELEASE]"
      val result = Log4JParser.parse(input)

      result must be_===(List(StackTraceLine(
        channel = "java",
        Pcmfl(
          `package` = "org.springframework.jdbc.datasource",
          `class` = "DataSourceUtils",
          method = "getConnection",
          file = Some("DataSourceUtils.java"),
          line = Some(80),
          isNative = false,
          isUnknownSource = false
        ),
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
          Pcmfl(
            `package` = "org.springframework.jdbc.datasource",
            `class` = "DataSourceUtils",
            method = "getConnection",
            file = Some("DataSourceUtils.java"),
            line = Some(80),
            isNative = false,
            isUnknownSource = false
          ),
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

  """[de.cas.open.dbassistent_1.0.0.v20140704-1337.jar:na]""" should (
    "be parsed as jar(de.cas.open.dbassistent_1.0.0.v20140704-1337.jar:na)" in {

      val input = "[de.cas.open.dbassistent_1.0.0.v20140704-1337.jar:na]"

      val result = Log4JParser.parseChunk(Log4JParser.jar, input)

      result must be_==("de.cas.open.dbassistent_1.0.0.v20140704-1337.jar:na")
    })

  """sun.reflect.NativeConstructorAccessorImpl.newInstance0(Native Method)""" should (
    """be parsed as Pcmfl(
      |          `package` = "sun.reflect",
      |          `class` = "NativeConstructorAccessorImpl",
      |          method = "newInstance0",
      |          file = None,
      |          line = None,
      |          isNative = true,
      |          isUnknownSource = false
      |       ))""".stripMargin in {

      val input = "sun.reflect.NativeConstructorAccessorImpl.newInstance0(Native Method)"

      val result = Log4JParser.parseChunk(Log4JParser.pcmfl, input)

      result must be_==(Pcmfl(
        `package` = "sun.reflect",
        `class` = "NativeConstructorAccessorImpl",
        method = "newInstance0",
        file = None,
        line = None,
        isNative = true,
        isUnknownSource = false
      ))
    })

  """(Native Method)""" should (
    """be parsed as 'Native Method'""" in {

      val input = "(Native Method)"

      val result = Log4JParser.parseChunk(Log4JParser.fileOrNativeOrUnknown, input)

      result must be_==("(Native Method)")
    })

  """org.springframework.jdbc.core.JdbcTemplate.execute(JdbcTemplate.java:572)""" should (
    """be parsed as Pcmfl(
      |        `package` = "org.springframework.jdbc.core",
      |        `class` = "JdbcTemplate",
      |        method = "execute",
      |        file = Some("JdbcTemplate.java"),
      |        line = Some(572),
      |        isNative = false,
      |        isUnknownSource = false
      |      )""".stripMargin in {

      val input = "org.springframework.jdbc.core.JdbcTemplate.execute(JdbcTemplate.java:572)"

      val result = Log4JParser.parseChunk(Log4JParser.pcmfl, input)

      result must be_==(Pcmfl(
        `package` = "org.springframework.jdbc.core",
        `class` = "JdbcTemplate",
        method = "execute",
        file = Some("JdbcTemplate.java"),
        line = Some(572),
        isNative = false,
        isUnknownSource = false
      ))
    })

  """        [echo] tmpdir=C:\Users\JANOS~1.SET\AppData\Local\Temp\  casopen.serverbuild.tmp=C:\Users\JANOS~1.SET\AppData\Local\Temp\/casopenbuild """ should (
    """be parsed as List(EchoProperties(
      |        List(
      |          ("tmpdir","C:\\Users\\JANOS~1.SET\\AppData\\Local\\Temp\\"),
      |          ("casopen.serverbuild.tmp","C:\\Users\\JANOS~1.SET\\AppData\\Local\\Temp\\/casopenbuild")
      |        )
      |      ))""".stripMargin in {

      val input = """        [echo] tmpdir=C:\Users\JANOS~1.SET\AppData\Local\Temp\  casopen.serverbuild.tmp=C:\Users\JANOS~1.SET\AppData\Local\Temp\/casopenbuild """

      val result = Log4JParser.parse(input)

      result must be_==(List(EchoProperties(
        List(
          ("tmpdir", "C:\\Users\\JANOS~1.SET\\AppData\\Local\\Temp\\"),
          ("casopen.serverbuild.tmp", "C:\\Users\\JANOS~1.SET\\AppData\\Local\\Temp\\/casopenbuild")
        )
      )))
    })

  """The "mixed log4j and ant.log" file""" should (
    "be parsed" in {

      val input = Source.fromFile("src/test/resources/mixed log4j and ant.log").bufferedReader()

      val result = Log4JParser.parse(input)

      val inputReadAgain = Source.fromFile("src/test/resources/truncated mixed log4j and ant.log").iter
      result.count(!_.isInstanceOf[EmptyLine]) must be_==(inputReadAgain.length)
    })
}

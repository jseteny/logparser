# logparser
Log parser in Scala for ant build results, log4j and logback

```scala
val input = " [java] 15:13:29, INFO, , d.c.o.d.internal.DBAssistent - Processing global schema 'globalschema'."
val result = Log4JParser.parse(input)

result must be_===(List(LogLine(
  channel = "java",
  time = "15:13:29",
  category = "INFO",
  rest = ", d.c.o.d.internal.DBAssistent - Processing global schema 'globalschema'."
)))

...

val input = " [java] \tat org.springframework.jdbc.datasource.DataSourceUtils.getConnection(DataSourceUtils.java:80) ~[org.springframework.jdbc_3.0.5.RELEASE.jar:3.0.5.RELEASE]"
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

```
Please see https://github.com/jseteny/logparser/blob/master/src/test/scala/hu/matan/log/parser/LogParserSpec.scala

name := "LogParser"

version := "1.0"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "2.4.10" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")

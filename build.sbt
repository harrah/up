scalaVersion := "2.10.4"

crossScalaVersions := Seq("2.10.4", "2.11.1")

name := "up"

scalacOptions ++= Seq(
  "-feature",
  "-explaintypes"
)

libraryDependencies += "org.specs2" %% "specs2" % "2.3.12" % "test"

initialCommands in console := "import up._, Dense._, Bool._, HList._"

scalaVersion := "2.10.2"

name := "up"

scalacOptions ++= Seq(
  "-feature",
  "-explaintypes"
)

libraryDependencies += "org.specs2" %% "specs2" % "2.0" % "test"

initialCommands in console := "import up._, Dense._, Bool._, HList._"

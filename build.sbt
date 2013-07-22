scalaVersion := "2.10.2"

name := "up"

scalacOptions ++= Seq(
  "-feature",
  "-explaintypes"
)

initialCommands in console := "import up._, Dense._, Bool._, HList._"

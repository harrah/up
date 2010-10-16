import sbt._

class T(info: ProjectInfo) extends DefaultProject(info)
{
	override def compileOptions = super.compileOptions ++ compileOptions("-explaintypes")
	override def consoleInit =
"""import Dense._
import Bool._
import HList._"""
}

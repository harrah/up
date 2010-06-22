import Nat._
import Bool._

object NatTest
{
	val true = toBoolean[ Sq[_9] === ( _1 + (_8 x _10) )]
	val true = toBoolean[ Sq[Sq[_9]] === Sq[_1 + (_8 x _10) ] ]
	val true = toBoolean[ ( (_9 ^ _4) % _6) === _3 ]
	val true = toInt[ ( Sq[_9] % _6 ) ] == 81 % 6
	def run() {}
}
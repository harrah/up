package up

import org.specs2.Specification
import Nat._
import Bool._

class NatTest extends Specification {
  def is = "Nat should"      ^
    "work as expected" ! run ^
                         end

  val true = toBoolean[ Sq[_9] === ( _1 + (_8 x _10) )]
  //val true = toBoolean[ Sq[Sq[_9]] === Sq[_1 + (_8 x _10) ] ]
  //val true = toBoolean[ ( (_9 ^ _4) % _6) === _3 ]
  val true = toInt[ ( Sq[_9] % _6 ) ] == 81 % 6

  def run = ok
}

package up

import org.specs2.Specification
import HList._
import scala.xml.NodeSeq

class UpdateTest extends Specification {
  def is = "Update should"   ^
    "work as expected" ! run ^
                         end

  val h = 3 :: false :: "asdf" :: 'c' :: 9.0 :: List(1,2,3) ::
    Map(1 -> 2, 3 -> 4) :: (<x><asdf/></x> : NodeSeq) :: HNil

  val g1 = (_: NodeSeq :: String :: HNil) match { case e :: s :: HNil =>
    (e \ s) :: HNil
  }

  val g2 = (_: Int :: List[Int] :: Char :: Map[Int,Int] :: HNil) match { case i :: l :: c :: m :: HNil =>
    (m(i) + l.sum) :: (c + m(l.head).toString) :: HNil
  }

  val u1 = h update g1

  val u2 = h update g2

  def run = ok
}

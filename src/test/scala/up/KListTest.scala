package up

import org.specs2.Specification
import KList._
import HList.{:: => :+: }
import ~>._


class KListTest extends Specification {
  def is = "KList should"    ^
    "work as expected" ! run ^
                         end

  val f = new (Option ~> List) { def apply[T](o: Option[T]): List[T] = o.toList }

  val x = Some(3) :^: Some("asdf") :^: KNil
  val y = x map f
  y match { case List(3) :^: List("asdf") :^: KNil => println("true") }

  val head = new (List ~> Id) { def apply[T](xs: List[T]): T = xs.head }
  val z = y down head
  // pattern matching with :^: without 'down' doesn't work (Id again)
  def run = {
    z match { case 3 :+: "asdf" :+: HNil => println("true") }
    ok
  }
}

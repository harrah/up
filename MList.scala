import MList._

import HList.{:: => :+: }
import ~>._

sealed trait MList[+M[_]] {

  // For converting MList[Id] to an HList
  //  This is useful because type inference doesn't work well with Id
  type Raw <: HList
  def down(implicit ev: M ~> Id): Raw

  // For natural transformations
  type Map[N[_]] <: MList[N]
  def map[N[_]](f: M ~> N): Map[N]

  // convert to a regular List
  def toList: List[M[_]]
}
final case class MCons[H, +T <: MList[M], +M[_]](head: M[H], tail: T) extends MList[M] {

  type Raw = H :+: tail.Raw
  def down(implicit f: M ~> Id): Raw = HCons(f(head), tail.down(f))

  type Map[N[_]] = MCons[H, tail.Map[N], N]
  def map[N[_]](f: M ~> N) = MCons( f(head), tail.map(f) )

  // prepend
  def :^: [N[X] >: M[X], G](g: N[G]): MCons[G, MCons[H, T, N], N] = MCons(g, this)

  def toList = head :: tail.toList
}
sealed class MNil extends MList[Nothing] {

  type Raw = HNil
  def down(implicit f: Nothing ~> Id) = HNil

  type Map[N[_]] = MNil
  def map[N[_]](f: Nothing ~> N) = MNil

  def :^: [M[_], H](h: M[H]): MCons[H, MNil, M] = MCons(h, this)

  def toList = Nil
}
object MNil extends MNil

object MList {
  // nicer alias for pattern matching
  val :^: = MCons
}


object MTest {
  
  val f = new (Option ~> List) { def apply[T](o: Option[T]): List[T] = o.toList }
  
  val x = Some(3) :^: Some("asdf") :^: MNil
  val y = x map f
  y match { case List(3) :^: List("asdf") :^: MNil => println("true") }

  val head = new (List ~> Id) { def apply[T](xs: List[T]): T = xs.head }
    // Id doesn't get inferred
  val z = y.map[Id](head).down
    // pattern matching with :^: without 'down' doesn't work (Id again)
  z match { case 3 :+: "asdf" :+: HNil => println("true") }
}
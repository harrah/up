import HList.{:: => :+: }
import ~>._
import KList._

/** A higher-order heterogeneous list.  It has a type constructor M[_] and
* type parameters HL.  The underlying data is M applied to each type parameter.
* Explicitly tracking M[_] allows performing natural transformations or ensuring
* all data conforms to some common type. */
sealed trait KList[+M[_], HL <: HList] {
	type Raw = HL
	/** Convert to the underlying HList type.*/
	def down(implicit ev: M ~> Id): HL
	/** Apply a natural transformation. */
	def map[N[_]](f: M ~> N): KList[N, HL]
	/** Convert to a List. */
	def toList: List[M[_]]
	def combine[N[X] >: M[X]]: HL#Wrap[N]
}

final case class KCons[H, T <: HList, +M[_]](head: M[H], tail: KList[M,T]) extends KList[M, H :+: T] {
	def down(implicit f: M ~> Id) = HCons(f(head), tail.down(f))
	def map[N[_]](f: M ~> N) = KCons( f(head), tail.map(f) )
	// prepend
	def :^: [N[X] >: M[X], G](g: N[G]) = KCons(g, this)
	def toList = head :: tail.toList
	
	def combine[N[X] >: M[X]]: (H :+: T)#Wrap[N] = HCons(head, tail.combine)
}

sealed class KNil extends KList[Nothing, HNil] {
	def down(implicit f: Nothing ~> Id) = HNil
	def map[N[_]](f: Nothing ~> N) = KNil
	def :^: [M[_], H](h: M[H]) = KCons(h, this)
	def toList = Nil
	def combine[N[X]] = HNil
}
object KNil extends KNil

object KList
{
	// nicer alias for pattern matching
	val :^: = KCons
}


object KTest {
  
  val f = new (Option ~> List) { def apply[T](o: Option[T]): List[T] = o.toList }
  
  val x = Some(3) :^: Some("asdf") :^: KNil
  val y = x map f
  y match { case List(3) :^: List("asdf") :^: KNil => println("true") }

  val head = new (List ~> Id) { def apply[T](xs: List[T]): T = xs.head }
  val z = y down head
    // pattern matching with :^: without 'down' doesn't work (Id again)
  z match { case 3 :+: "asdf" :+: HNil => println("true") }
}
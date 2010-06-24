import HList._
import Nat._

// heterogeneous list
sealed trait HList
{
	type Head
	type Tail <: HList
	type Length = Foldr[Nat, Fold.Inc, _0]
	type Wrap[M[_]] <: HList
	
	type Foldr[Value, F <: Fold[Any, Value], I <: Value] <: Value
	def foldr[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I): Foldr[Value, F, I]

	type Foldl[Value, F <: Fold[Any, Value], I <: Value] <: Value
	def foldl[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I): Foldl[Value, F, I]

	type toI[N <: Nat] <: Indexed
}

final case class HCons[H, T <: HList](head : H, tail : T) extends HList
{
	type Wrap[M[_]] = HCons[ M[H], T#Wrap[M] ]
	type Head = H
	type Tail = T
	def ::[T](v : T) = HCons(v, this)
	
	type Foldr[Value, F <: Fold[Any, Value], I <: Value] = F#Apply[H, tail.Foldr[Value, F, I]]
	def foldr[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I): Foldr[Value, F, I] =
		f(head, tail.foldr[Value, F, I](f, i) )

	type Foldl[Value, F <: Fold[Any, Value], I <: Value] = tail.Foldl[Value, F, F#Apply[H, I]]
	def foldl[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I): Foldl[Value, F, I] =
		tail.foldl[Value, F, F#Apply[H,I]](f, f(head, i))

	type IN[M <: Nat] = IndexedN[H, tail.toI[M]]
	type toI[N <: Nat] = N#Match[ IN, Indexed0[H, T], Indexed]
	
	override def toString = head + " :: " + tail
}
sealed class HNil extends HList
{
	type Head = Nothing
	type Tail = HNil
	type Wrap[M[_]] = HNil
	def ::[T](v : T) = HCons(v, this)

	type Foldl[Value, F <: Fold[Any, Value], I <: Value] = I
	def foldl[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I) = i

	type Foldr[Value, F <: Fold[Any, Value], I <: Value] = I
	def foldr[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I) = i

	type toI[N <: Nat] = Nothing
}
case object HNil extends HNil

object HList extends HApplyOps with UnzipOps
{
	// type alias for writing HCons[H, T] as H :: T
	type ::[H, T <: HList] = HCons[H, T]
	// extractor for writing
	//  case HCons(head, tail) =>
	//as
	//  case head :: tail =>
	object :: {
		def unapply[H,T<:HList](list: HCons[H,T]) = Some((list.head,list.tail))
	}
	
	def toList[S] = new ToList[S]
	implicit def listHCons[H <: S, T <: HList, S](implicit f: T => List[S]): (H :: T) => List[S] = hc => hc.head :: f(hc.tail)
	implicit def listHNil[S]: HNil => List[S] = hn => Nil
	
	import Indexed._
	
	type :::[A <: HList, B <: HList] = A#Foldr[HList, AppHCons.type, B]
	type Reverse_:::[A <: HList, B <: HList] = A#Foldl[HList, AppHCons.type, B]
	type Reverse[A <: HList] = A#Foldl[HList, AppHCons.type, HNil]

	object Concat extends Fold[HList, HList] {
		type Apply[N <: HList, H <: HList] = N ::: H
		def apply[A <: HList, B <: HList](a: A, b: B) = a ::: b
	}

	implicit def hlistOps[B <: HList](b: B): HListOps[B] =
		new HListOps[B] {
			def length = b.foldr(Length, 0)
			def reverse = b.foldl[HList, AppHCons.type, HNil](AppHCons, HNil)
			def :::[A <: HList](a: A): A#Foldr[HList, AppHCons.type, B] =
				a.foldr[HList, AppHCons.type, B](AppHCons, b)
			def reverse_:::[A <: HList](a: A): A Reverse_::: B =
				a.foldl[HList, AppHCons.type, B](AppHCons, b)
			def zip[C <: HList, R <: HList](c: C)(implicit hzip: HZip[B,C, R]): R = hzip(b, c)
			def ::[A](a: A) = HCons(a, b)
			//def concat = b.foldr(Concat, HNil)
		}

	object AppHCons extends Fold[Any, HList] {
		type Apply[N <: Any, H <: HList] = N :: H
		def apply[A,B <: HList](a: A, b: B) = HCons(a, b)
	}

	import Nat._
	object Length extends Fold[Any, Int] {
		type Apply[N <: Any, Acc <: Int] = Int
		def apply[A,B <: Int](a: A, b: B) = b+1
	}
	implicit def hconsOps[H, T <: HList](h: H :: T): HConsOps[H, T] =
		new HConsOps[H, T] {
			def hlist = h
			def last = h.tail.foldl[Any, Last.type, H](Last, h.head)
			def i[N <: Nat](implicit i: H::T => Ind[N]) = i(h)
		}

	object Last extends Fold[Any, Any] {
		type Apply[N <: Any, H <: Any] = N
		def apply[A,B](a: A, b: B) = a
	}
}


sealed trait HConsOps[H, T <: HList] {
	def last: Last
	def hlist: H :: T
	type Last = T#Foldl[Any, Last.type, H]
	def i[N <: Nat](implicit it: H::T => Ind[N]): Ind[N]

	type Ind[N <: Nat] = HCons[H, T]#toI[N]
}
sealed trait HListOps[B <: HList] {
	def length: Int
	def :::[A <: HList](a: A): A ::: B
	def reverse: Reverse[B]
	def reverse_:::[A <: HList](a: A): A Reverse_::: B
	def ::[A](b: A): A :: B

	def zip[C <: HList, R <: HList](c: C)(implicit hzip: HZip[B,C, R]): R
}
final class ToList[S] {
  def apply[H <: HList](h: H)(implicit f: H => List[S]): List[S] = f(h)
}
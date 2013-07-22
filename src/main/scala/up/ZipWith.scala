package up


import HList.{:: => :+: }
import ~>._

object ZipWith {
  def zipWith[HL <: HList, T](kl: KList[Stream, HL])(f: HL => T): Stream[T] =
    if(anyEmpty(kl))
      Stream.empty
    else
      Stream.cons( f( kl down heads ), zipWith(kl map tails )(f) )

  def foreach[HL <: HList, T](kl: KList[Stream, HL])(f: HL => T): Unit =
    if(anyEmpty(kl))
      ()
    else {
      f( kl down heads )
      foreach(kl map tails )(f)
    }

  def collect[HL <: HList, T](kl: KList[Stream, HL])(f: HL => Option[T]): Stream[T] =
    if(anyEmpty(kl))
      Stream.empty
    else {
      lazy val ts = kl map tails
      f( kl down heads ) match {
        case Some(v) => Stream.cons(v, collect( ts )(f) )
        case None => collect( ts )(f) // recurse directly on collect for tail position
      }
    }

  def flatMap[HL <: HList, T](kl: KList[Stream, HL])(f: HL => Stream[T]): Stream[T] =
    zipWith(kl)(f).flatten

  def forall[HL <: HList](kl: KList[Stream, HL])(f: HL => Boolean): Boolean =
    zipWith(kl)(f).forall(identity[Boolean])

  def exists[HL <: HList](kl: KList[Stream, HL])(f: HL => Boolean): Boolean =
    zipWith(kl)(f).exists(identity[Boolean])

  def anyEmpty(kl: KList[Stream, _]): Boolean = kl.toList.exists(_.isEmpty)

  val heads = new (Stream ~> Id) { def apply[T](s: Stream[T]): T = s.head }
  val tails = new (Stream ~> Stream) { def apply[T](s: Stream[T]): Stream[T] = s.tail }
}

trait ZippedK[S[_], HL <: HList] {
  def zipWith[T](f: HL => T): S[T]
  def exists(f: HL => Boolean): Boolean
  def forall(f: HL => Boolean): Boolean
  def collect[T](f: HL => Option[T]): S[T]
  def foreach[T](f: HL => T): Unit
  def flatMap[T](f: HL => S[T]): S[T]
}

object ZippedK {
  implicit def zippedK[HL <: HList](in: KList[Stream, HL]): ZippedK[Stream, HL] = new ZippedK[Stream, HL] {
    def zipWith[T](f: HL => T): Stream[T] = ZipWith.zipWith(in)(f)
    def exists(f: HL => Boolean): Boolean = ZipWith.exists(in)(f)
    def forall(f: HL => Boolean): Boolean = ZipWith.forall(in)(f)
    def collect[T](f: HL => Option[T]): Stream[T] = ZipWith.collect(in)(f)
    def foreach[T](f: HL => T): Unit = ZipWith.foreach(in)(f)
    def flatMap[T](f: HL => Stream[T]): Stream[T] = ZipWith.flatMap(in)(f)
  }

  type S[A] = Stream[A]

  implicit def zipped2[A,B](t: (S[A],S[B]) ) = zippedK(t._1 :^: t._2 :^: KNil)
  implicit def zipped3[A,B,C](t: (S[A],S[B],S[C]) ) = zippedK(t._1 :^: t._2 :^: t._3 :^: KNil)
  implicit def zipped4[A,B,C,D](t: (S[A],S[B],S[C],S[D]) ) = zippedK(t._1 :^: t._2 :^: t._3 :^: t._4 :^: KNil)
  implicit def zipped5[A,B,C,D,E](t: (S[A],S[B],S[C],S[D],S[E]) ) =
    zippedK(t._1 :^: t._2 :^: t._3 :^: t._4 :^: t._5 :^: KNil)
  implicit def zipped6[A,B,C,D,E,F](t: (S[A],S[B],S[C],S[D],S[E],S[F]) ) =
    zippedK(t._1 :^: t._2 :^: t._3 :^: t._4 :^: t._5 :^: t._6 :^: KNil)

  implicit def f2ToH[A,B,R](f: (A,B) => R) = pf[A :+: B :+: HNil, R]{ case av :+: bv :+: HNil => f(av, bv) }
  implicit def f3ToH[A,B,C,R](f: (A,B,C) => R) = pf[A :+: B :+: C :+: HNil, R] {
    case av :+: bv :+: cv :+: HNil => f(av, bv, cv)
  }
    implicit def f4ToH[A,B,C,D,R](f: (A,B,C,D) => R) = pf[A :+: B :+: C :+: D :+: HNil, R] {
      case av :+: bv :+: cv :+: dv :+: HNil => f(av, bv, cv, dv)
    }
    implicit def f5ToH[A,B,C,D,E,R](f: (A,B,C,D,E) => R) = pf[A :+: B :+: C :+: D :+: E :+: HNil, R] {
      case av :+: bv :+: cv :+: dv :+: ev :+: HNil => f(av, bv, cv, dv, ev)
    }
    implicit def f6ToH[A,B,C,D,E,F,R](f: (A,B,C,D,E,F) => R) = pf[A :+: B :+: C :+: D :+: E :+: F :+: HNil, R] {
      case av :+: bv :+: cv :+: dv :+: ev :+: fv :+: HNil => f(av, bv, cv, dv, ev, fv)
    }

  def pf[H <: HList, R](f: H => R) = f
}

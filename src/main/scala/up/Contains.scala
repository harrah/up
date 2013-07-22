package up


import HList._

sealed trait Contained[HL <: HList, A]
sealed trait NotContained[HL <: HList, A]

object Contained {
    implicit def tailContains[A, H, T <: HList](implicit c: Contained[T, A]): Contained[H :: T, A] = impl
    implicit def contains[A, H, T <: HList](implicit x: A =:= H): Contained[H :: T, A] = impl

    private[this] def impl[H <: HList, A] = new Contained[H, A] {}
}

object NotContained {
    implicit def nilDoesNotContain[A]: NotContained[HNil, A] = impl
    implicit def doesNotContain[H, T <: HList, A]
      (implicit noTail: NotContained[T, A]): NotContained[H :: T, A] = impl
    implicit def ambiguousContains[H, T <: HList, A]
      (implicit contains: A =:= H, noTail: NotContained[T, A]): NotContained[H :: T, A] = impl

    private[this] def impl[H <: HList, A] = new NotContained[H, A] {}
}

object Contains {
    def mustContain[A, HL <: HList](implicit c: Contained[HL, A]) = true
    def mustNotContain[A, HL <: HList](implicit c: NotContained[HL, A]) = false

    def excludeDemo[A](a: A)(implicit c: NotContained[Int :: Boolean :: Double :: HNil, A]) = a
}

import Contains._
object Work {
    mustContain[Int, Int :: Double :: Boolean :: HNil]
    mustContain[Int, Double :: Boolean :: Int :: HNil]
    mustNotContain[Int, Double :: Boolean :: HNil]

    excludeDemo("works")
    excludeDemo(3.0f)
    excludeDemo( () )
}

object Fail {
    /*mustNotContain[Int, Int :: Double :: Boolean :: HNil]
    mustNotContain[Int, Double :: Boolean :: Int :: HNil]
    mustContain[Int, Double :: Boolean :: HNil]

    excludeDemo(3)
    excludeDemo(false)
    excludeDemo(9.3)*/
}

package up


sealed trait HZip[A <: HList, B <: HList, Result <: HList] {
	def apply(a: A, b: B): Result
}
object HZip
{
	import HList.::
	implicit def hzipNil0: HZip[HNil, HNil, HNil] = new HZip[HNil, HNil, HNil] { def apply(a: HNil, b: HNil) = HNil }
	implicit def hzipNil1[H, T <: HList]: HZip[HCons[H,T], HNil, HNil] = new HZip[HCons[H,T], HNil, HNil] { def apply(a: HCons[H,T], b: HNil) = HNil }
	implicit def hzipNil2[H, T <: HList]: HZip[HNil, HCons[H,T], HNil] = new HZip[HNil, HCons[H,T], HNil] { def apply(a: HNil, b: HCons[H,T]) = HNil }

	implicit def hzipCons[HA, HB, TA <: HList, TB <: HList, TR <: HList](implicit hzipTail: HZip[TA, TB, TR]): HZip[HA :: TA, HB :: TB, (HA, HB) :: TR] =
		new HZip[HA :: TA, HB :: TB, (HA, HB) :: TR] {
			def apply(a: HA :: TA, b: HB :: TB) = HCons( (a.head, b.head), hzipTail(a.tail, b.tail) )
		}
}
	import ~>.Id
	import KList.:^:
	import HList.{::, :::}
	import Select.KSelect

final class TypeSelect[Data <: HList](data: Data)
{
	type select[S] = Select[Data, S]
	type kselect[HL <: HList] = KList[select, HL]

	val extract = new (select ~> Id) {
		def apply[S](in: select[S]): S =
			in(data)
	}

	def update[In <: HList, Out <: HList](f: In => Out)(
		implicit ik: kselect[In],
		ok: kselect[Out]): Data =
	{
		val in = ik down extract
		val out = f(in)
		replace(out, data, ok)
	}
	def replace[HL <: HList](out: HL, data: Data, f: kselect[HL]): Data =
		(out, f) match
		{
			case ( nh :: nt, hf :^: tf ) => replace(nt, update0(nh, data, hf), tf)
			case ( HNil, KNil ) => data
		}
	def update0[S](value: S, data: Data, tip: select[S]): Data =
		tip.update(data, value)
}
sealed trait Select[Data <: HList, At] {
	def apply(data: Data): At
	def update(data: Data, value: At): Data
}

object Select
{
	sealed trait select[Data <: HList] { type Apply[A] = Select[Data, A] }
	type KSelect[Data <: HList, HL <: HList] = KList[ select[Data]#Apply, HL]

	implicit def consSelect[H, T <: HList, Data <: HList](implicit headSelect: Select[Data, H], tailSelect: KSelect[Data, T]): KSelect[Data, H :: T] =
		KCons[H, T, select[Data]#Apply](headSelect, tailSelect)
	implicit def nilSelect[Data <: HList]: KSelect[Data, HNil] = KNil

	implicit def select0[H, T <: HList]: Select[H :: T, H] = new Select[H :: T, H] {
		def apply(data: H :: T): H = data.head
		def update(data: H :: T, value: H): H :: T = value :: data.tail
	}
	implicit def selectN[At, H, T <: HList, S <: Select[T, At]](implicit tail: S): Select[H :: T, At] = new Select[H :: T, At] {
		def apply(data: H :: T): At = tail(data.tail)
		def update(data: H :: T, value: At): H :: T = data.head :: tail.update(data.tail, value)
	}
}
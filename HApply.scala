import HList.::

sealed trait HApply[-In, +Out] { def apply(in: In): Out }
trait HApplyOps
{
	implicit def happlyNil(h: HNil) : HApply[HNil, HNil] =
		new HApply[HNil, HNil] { def apply(in: HNil) = HNil }
		
	implicit def happlyCons[InH, OutH, TF <: HList, TIn <: HList, TOut <: HList]
		(implicit applyTail: TF => HApply[TIn, TOut]):
			HCons[InH => OutH, TF] => HApply[InH :: TIn, OutH :: TOut] = h => 
			
		new HApply[InH :: TIn, OutH :: TOut] {
			def apply(in: InH :: TIn) =
				HCons( h.head(in.head), applyTail(h.tail)(in.tail) )
		}
	// required because HCons isn't covariant in H
	implicit def happlyCons2[InH, OutH, TF <: HList, TIn <: HList, TOut <: HList]
		(implicit applyTail: TF => HApply[TIn, TOut]):
			HCons[HApply[InH, OutH], TF] => HApply[InH :: TIn, OutH :: TOut] = h => 
			
		new HApply[InH :: TIn, OutH :: TOut] {
			def apply(in: InH :: TIn) =
				HCons( h.head(in.head), applyTail(h.tail)(in.tail) )
		}
		
	def partial[H <: HList, In <: HList, Out <: HList](h: H)(implicit toApply: H => HApply[In, Out]): HApply[In, Out] =
		toApply(h)
	def happly[H <: HList, In <: HList, Out <: HList](h: H)(in: In)(implicit toApply: H => HApply[In, Out]): Out =
		toApply(h)(in)
}

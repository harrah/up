package up


import HList.::

trait HApplyOps {
  implicit def happlyNil(h: HNil) : HNil => HNil = _ => HNil

  implicit def happlyCons[InH, OutH, TF <: HList, TIn <: HList, TOut <: HList]
    (implicit applyTail: TF => TIn => TOut):
      ((InH => OutH) :: TF) => ( (InH :: TIn) => (OutH :: TOut) ) = h =>
        in =>   HCons( h.head(in.head), applyTail(h.tail)(in.tail) )

  def partial[H <: HList, In <: HList, Out <: HList](h: H)(implicit toApply: H => In => Out): In => Out =
    toApply(h)
  def happly[H <: HList, In <: HList, Out <: HList](h: H)(in: In)(implicit toApply: H => In => Out): Out =
    toApply(h)(in)
}

trait Unzip[H <: HList, R1 <: HList, R2 <: HList] {
	def unzip(h: H): (R1, R2)
}
trait UnzipOps
{
	implicit def unzipNil : Unzip[HNil, HNil, HNil] =
		new Unzip[HNil, HNil, HNil] { def unzip(h: HNil) = (HNil, HNil) }
		
	implicit def unzipCons[H1, H2, T <: HList, TR1 <: HList, TR2 <: HList]
		(implicit unzipTail: Unzip[T, TR1, TR2]):
			Unzip[HCons[(H1,H2), T], HCons[H1, TR1], HCons[H2, TR2]] =
			
		new Unzip[HCons[(H1,H2), T], HCons[H1, TR1], HCons[H2, TR2]]  {
			def unzip(h: HCons[(H1,H2), T]) = {
				val (t1, t2) = unzipTail.unzip(h.tail)
				(HCons(h.head._1, t1), HCons(h.head._2, t2))
			}
		}
		
	def unzip[H <: HList, R1 <: HList, R2 <: HList](h: H)(implicit un: Unzip[H, R1, R2]): (R1, R2) =
		un unzip h
}
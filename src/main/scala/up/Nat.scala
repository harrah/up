package up


import Nat._

object Nat
{
	type _0 = Nat0
	type _1 = Succ[_0]
	type _2 = Succ[_1]
	type _3 = Succ[_2]
	type _4 = Succ[_3]
	type _5 = Succ[_4]
	type _6 = Succ[_5]
	type _7 = Succ[_6]
	type _8 = Succ[_7]
	type _9 = Succ[_8]
	type _10 = Succ[_9]
	
	type Compare[A <: Nat, B <: Nat] = A#Compare[B]
	type ===[A <: Nat, B <: Nat] = A#Compare[B]#eq
	type Is0[A <: Nat] = A#Match[ConstFalse, True, Bool]
		
	type +[A <: Nat, B <: Nat] = A#FoldR[B, Nat, Fold.Inc]
	type x[A <: Nat, B <: Nat] = A#FoldR[_0, Nat, Fold.Sum[B]]
	type Fact[A <: Nat] = A#FoldL[_1, Nat, Fold.Prod]
	type ^[A <: Nat, B <: Nat] = B#FoldR[_1, Nat, Fold.Exp[A]]
	type %[A <: Nat, B <: Nat] = A#FoldR[_0, Nat, Fold.Mod[B]]
	type Sq[A <: Nat] = A ^ _2
	
	type ConstLT[A] = LT
	type ConstFalse[A] = False
	
	implicit def natRep0: NatRep[_0] = new NatRep[_0](0)
	implicit def natRepN[N <: Nat](implicit rep: NatRep[N]): NatRep[Succ[N]] = rep.succ
	final class NatRep[N <: Nat](val value: Int) { def succ: NatRep[Succ[N]] = new NatRep[Succ[N]](value+1) }
	
	// for converting a type to a value.  Note that this will stack overflow for reasonably large numbers
	def toInt[N <: Nat](implicit rep: NatRep[N]): Int = rep.value
}



sealed trait Nat {
	type FoldR[Init <: Type, Type, F <: Fold[Nat, Type]] <: Type
	type FoldL[Init <: Type, Type, F <: Fold[Nat, Type]] <: Type
	type Compare[N <: Nat] <: Comparison
	type Match[NonZero[N <: Nat] <: Up, IfZero <: Up, Up] <: Up
}
sealed trait Nat0 extends Nat {
	type FoldR[Init <: Type, Type, F <: Fold[Nat, Type]] = Init
	type FoldL[Init <: Type, Type, F <: Fold[Nat, Type]] = Init
	type Match[NonZero[N <: Nat] <: Up, IfZero <: Up, Up] = IfZero
	type Compare[N <: Nat] = N#Match[ConstLT, EQ, Comparison]
}
sealed trait Succ[N <: Nat] extends Nat {
	type FoldR[Init <: Type, Type, F <: Fold[Nat, Type]] = F#Apply[Succ[N], N#FoldR[Init, Type, F]]
	type FoldL[Init <: Type, Type, F <: Fold[Nat, Type]] = N#FoldL[F#Apply[Succ[N],Init], Type, F]
	type Match[NonZero[N <: Nat] <: Up, IfZero <: Up, Up] = NonZero[N]
	type Compare[O <: Nat] = O#Match[N#Compare, GT, Comparison]
}

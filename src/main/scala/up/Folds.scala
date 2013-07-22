package up



// workaround for kind conformance error
trait Fold[-Elem, Value] {
	type Apply[N <: Elem, Acc <: Value] <: Value
	def apply[N <: Elem, Acc <: Value](n: N, acc: Acc): Apply[N, Acc]
}
/** Workaround for separate compilation issue with higher-kinded types:
* All Fold implementations need to be defined in the same file as Fold.*/
object Fold {
	import Nat._
	type Inc = Fold[Any, Nat] {
		type Apply[N <: Any, Acc <: Nat] = Succ[Acc]
	}
	type Sum[By <: Nat] = Fold[Nat, Nat] {
		type Apply[N <: Nat, Acc <: Nat] = By + Acc
	}
   type Prod = Fold[Nat, Nat] {
      type Apply[N <: Nat, Acc <: Nat] = N x Acc
   }
	type Exp[By <: Nat] = Fold[Nat, Nat] {
		type Apply[N <: Nat, Acc <: Nat] = By x Acc
	}
	type Mod[By <: Nat] = Fold[Nat, Nat] {
		type Wrap[Acc <: Nat] = By#Compare[Acc]#eq
		type Apply[N <: Nat, Acc <: Nat] = Wrap[Succ[Acc]]#If[_0, Succ[Acc], Nat]
	}
}
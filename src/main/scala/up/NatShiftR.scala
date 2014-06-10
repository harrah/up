package up


import Nat._

// Shift Right for Nat
trait ShiftR {
  trait P1of2[A[_ <: ShiftR,_], B <: ShiftR] { type Apply[C <: Nat] = A[B,C] }
  type Apply[S <: ShiftR, N <: Nat] <: Nat
}
trait ShiftR0 extends ShiftR {
  type Apply[S <: ShiftR, N <: Nat] =
    N#Match[ P1of2[NonZero, S]#Apply, _0, Nat]
  type NonZero[S <: ShiftR, N <: Nat] =
    N#Match[ P1of2[NonZero2, S]#Apply, _0, Nat]
  type NonZero2[S <: ShiftR, N <: Nat] =
    Succ[S#Apply[S, N]]
}
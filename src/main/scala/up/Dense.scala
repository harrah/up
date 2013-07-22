package up


import Dense._

sealed trait Dense {
	type digit <: Digit
	type tail <: Dense
	
	type Inc <: Dense
	type Add[b <: Dense] <: Dense
	type Dec <: Dense
	type ShiftR <: Dense
	type ShiftL <: Dense
	type Mult[b <: Dense] <: Dense
	type Sq <: Dense
	type Exp[b <: Dense] <: Dense
	
	type Reverse[Acc <: Dense] <: Dense
	
	type ReceiveMult[shifted <: Dense, acc <: Dense] <: Dense
	type ReceiveExp[base <: Dense, acc <: Dense] <: Dense
	type Match[ NonZero <: Up, Zero <: Up, Up] <: Up
	type Compare[B <: Dense] = CompareC[B, EQ]
	type CompareC[B <: Dense, Carry <: Comparison] <: Comparison
}
sealed trait DCons[d <: Digit, T <: Dense] extends Dense {
	type digit = d
	type tail = T
	
	type ShiftR = tail
	type ShiftL = Zero :: DCons[d, T]
	type Inc = d#Match[ Zero :: T#Inc, One :: T, Dense ]
	type Dec = d#Match[ T#Match[ Zero :: T, DNil, Dense], One :: T#Dec,  Dense ]
	
	type Add[b <: Dense] =
		b#Match[
			AddNZ[b],
			DCons[d,T],
			Dense]
	type AddNZ[b <: Dense] =
		d#Match[
			Add1[b],
			b#digit :: tail#Add[b#tail],
			Dense]
	type Add1[b <: Dense] =
		b#digit#Match[
			Zero :: tail#Add[b#tail]#Inc,
			d :: tail#Add[b#tail],
			Dense]
			
	type Reverse[Acc <: Dense] = T#Reverse[DCons[d, Acc]]

	type Mult[b <: Dense] =
		b#Compare[d :: T]#Match[ b#ReceiveMult[d :: T, DNil], ReceiveMult[b, DNil], ReceiveMult[b, DNil], Dense ]
	type ReceiveMult[shifted <: Dense, acc <: Dense] =
		Hack[tail#ReceiveMult[shifted#ShiftL, digit#Match[acc#Add[shifted], acc, Dense]]]
	// prevents a cyclic reference when doing: type A = _3#Mult[_3].
	type Hack[D <: Dense] = D
	
	type Sq = ReceiveMult[DCons[d, T], DNil]
	type Exp[b <: Dense] = b#ReceiveExp[DCons[d, T], _1]
	type ReceiveExp[b <: Dense, acc <: Dense] =
		tail#ReceiveExp[ b#Sq, digit#Match[ acc#Mult[b], acc, Dense] ]
	
	type Match[ NonZero <: Up, Zero <: Up, Up]  = NonZero

	type CompareC[B <: Dense, Carry <: Comparison] = B#Match[ CompareNZ[B, Carry], GT, Comparison]
	type CompareNZ[B <: Dense, C <: Comparison] =
		tail#CompareC[ B#tail, Carry[digit#Compare[B#digit], C] ]
	type Carry[New <: Comparison, C <: Comparison] = New#Match[ LT, C, GT, Comparison]
}
sealed trait DNil extends Dense {
	type tail = Nothing
	type digit = Nothing
	
	type Inc = One :: DNil
	type Dec = Nothing
	
	type ShiftR = DNil
	type ShiftL = DNil
	type Add[b <: Dense] = b
	type Mult[b <: Dense] = DNil
	type Sq = DNil
	
	type Reverse[D <: Dense] = D
	
	type Exp[b <: Dense] = b#Match[ _0, _1, Dense]
	type ReceiveExp[base <: Dense, acc <: Dense] = acc
	type ReceiveMult[shifted <: Dense, acc <: Dense] = acc
	
	type Match[ NonZero <: Up, Zero <: Up, Up] = Zero
	type CompareC[B <: Dense, C <: Comparison] = B#Match[ LT, C, Comparison]
}
object Dense
{
	type Compare[A <: Dense, B <: Dense] = A#Compare[B]
	type ::[H <: Digit, T <: Dense] = DCons[H, T]

	sealed trait Zero extends Digit {
		type Match[ IfOne <: Up, IfZero <: Up, Up] = IfZero
		type Compare[D <: Digit] = D#Match[ LT, EQ, Comparison]
	}
	sealed trait One extends Digit {
		type Match[ IfOne <: Up, IfZero <: Up, Up] = IfOne
		type Compare[D <: Digit] = D#Match[ EQ, GT, Comparison]
	}
	

	type _0 = DNil
	type _1 = One :: DNil
	type _2 = Zero :: One :: DNil
	type _3 = One :: One :: DNil
	type _4 = Zero :: Zero :: One :: DNil
	type _5 = One :: Zero :: One :: DNil
	type _6 = Zero :: One :: One :: DNil
	type _7 = One :: One :: One :: DNil
	type _8 = Zero :: Zero :: Zero :: One :: DNil
	type _9 = One :: Zero :: Zero :: One :: DNil
	type _10 = Zero :: One :: Zero :: One :: DNil
	type _11 = One :: One :: Zero :: One :: DNil
	type _12 = Zero :: Zero :: One :: One :: DNil
	type _13 = One :: Zero :: One :: One :: DNil
	type _14 = Zero :: One :: One :: One :: DNil
	type _15 = One :: One :: One :: One :: DNil

	final class DRep[D <: Dense](val value: Int)
	def toInt[ D <: Dense](implicit drep: DRep[D]): Int = drep.value
	implicit def dnilToRep = new DRep[DNil](0)
	implicit def dcons0ToRep[D <: Dense](implicit tailRep: DRep[D]): DRep[DCons[Zero, D]] = new DRep(tailRep.value * 2)
	implicit def dcons1ToRep[D <: Dense](implicit tailRep: DRep[D]): DRep[DCons[One, D]] = new DRep(tailRep.value * 2 + 1)

	def toTuple3[T <: Triple[Dense,Dense,Dense]](implicit dra: DRep[T#_1], drb: DRep[T#_2], drc: DRep[T#_3]) : (Int, Int, Int) =
		(toInt[T#_1], toInt[T#_2], toInt[T#_3])
}

sealed trait Digit {
	type Match[ IfOne <: Up, IfZero <: Up, Up] <: Up
	type Compare[D <: Digit] <: Comparison
}
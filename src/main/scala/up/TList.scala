package up


import TList._
sealed trait TList
{
	type Head <: Value
	type Tail <: TL[Value]
	type Value

	type Match[NonEmpty[H <: Value, T <: TL[Value]] <: Up, Empty <: Up, Up] <: Up
	type Foldr[Acc, F <: Fold[Value, Acc], I <: Acc] <: Acc
	type Foldl[Acc, F <: Fold[Value, Acc], I <: Acc] <: Acc
}
sealed trait TCons[H <: T#Value, T <: TList] extends TList
{
	type Head = H
	type Tail = T
	type Value = T#Value
	type Match[NonEmpty[H <: Value, T <: TL[Value]] <: Up, Empty <: Up, Up] = NonEmpty[H, T]
	type Foldr[Acc, F <: Fold[Tail#Value, Acc], I <: Acc] = F#Apply[H, Tail#Foldr[Acc, F, I]]
	type Foldl[Acc, F <: Fold[Tail#Value, Acc], I <: Acc] = Tail#Foldl[Acc, F, F#Apply[H, I]]
}
sealed trait TNil[V] extends TList
{
	type Head = Nothing
	type Tail = Nothing
	type Value = V
	type Match[NonEmpty[H <: Value, T <: TL[Value]] <: Up, Empty <: Up, Up] = Empty
	type Foldr[Acc, F <: Fold[Value, Acc], I <: Acc] = I
	type Foldl[Acc, F <: Fold[Value, Acc], I <: Acc] = I
}
object TList
{
	type ::[H <: T#Value, T <: TList] = TCons[H, T]
	type TL[V] = TList { type Value <: V }
}
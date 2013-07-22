package up


sealed trait Bool {
    type If[T <: Up, F <: Up, Up] <: Up
}
sealed trait True extends Bool {
    type If[T <: Up, F <: Up, Up] = T
}
sealed trait False extends Bool {
    type If[T <: Up, F <: Up, Up] = F
}
object Bool
{
    type &&[A <: Bool, B <: Bool] = A#If[B, False, Bool]
    type || [A <: Bool, B <: Bool] = A#If[True, B, Bool]
    type Not[A <: Bool] = A#If[False, True, Bool]

    final class BoolRep[B <: Bool](val value: Boolean)
    implicit val falseRep: BoolRep[False] = new BoolRep(false)
    implicit val trueRep: BoolRep[True] = new BoolRep(true)
    def toBoolean[B <: Bool](implicit b: BoolRep[B]): Boolean = b.value
}

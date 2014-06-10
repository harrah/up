package up


sealed trait Comparison {
    type gt <: Bool
    type ge <: Bool
    type eq <: Bool
    type le <: Bool
    type lt <: Bool
    type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] <: Up
}

sealed trait GT extends Comparison {
    type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfGT
    type eq = False
    type gt = True
    type lt = False
    type le = False
    type ge = True
}

sealed trait LT extends Comparison {
    type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfLT
    type eq = False
    type gt = False
    type lt = True
    type le = True
    type ge = False
}

sealed trait EQ extends Comparison {
    type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfEQ
    type eq = True
    type gt = False
    type lt = False
    type le = True
    type ge = True
}

object Comparison {
    def show[C <: Comparison](implicit rep: ComparisonRep[C]): String = rep.value
    implicit def eqToRep: ComparisonRep[EQ] = new ComparisonRep[EQ]("eq")
    implicit def ltToRep: ComparisonRep[LT] = new ComparisonRep[LT]("lt")
    implicit def gtToRep: ComparisonRep[GT] = new ComparisonRep[GT]("gt")
    final class ComparisonRep[+C <: Comparison](val value: String)
}

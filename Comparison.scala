sealed trait Comparison {
	type gt = Match[False, False, True, Bool]
	type ge = Match[False, True, True, Bool]
	type eq = Match[False, True, False, Bool]
	type le = Match[True, True, False, Bool]
	type lt = Match[True, False, False, Bool]
	type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] <: Up
}
sealed trait GT extends Comparison {
	type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfGT
}
sealed trait LT extends Comparison {
	type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfLT
}
sealed trait EQ extends Comparison {
	type Match[IfLT <: Up, IfEQ <: Up, IfGT <: Up, Up] = IfEQ
}
object Comparison {
	def show[C <: Comparison](implicit rep: ComparisonRep[C]): String = rep.value
	implicit def eqToRep: ComparisonRep[EQ] = new ComparisonRep[EQ]("eq")
	implicit def ltToRep: ComparisonRep[LT] = new ComparisonRep[LT]("lt")
	implicit def gtToRep: ComparisonRep[GT] = new ComparisonRep[GT]("gt")
	final class ComparisonRep[+C <: Comparison](val value: String)
}
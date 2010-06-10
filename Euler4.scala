
	import Dense._

trait Pali {
	type Result = Dense//Triple[Dense, Dense, Dense]

	type Apply[max <: Dense, iMax <: Dense, jMax <: Dense, i <: Dense, j <: Dense, P <: Pali] <: Result
}
trait Pali0 extends Pali {
	type Apply[max <: Dense, iMax <: Dense, jMax <: Dense, i <: Dense, j <: Dense, P <: Pali] =
		Apply1[max, iMax, jMax, i, j, j#Mult[i], P ]

	type Apply1[max <: Dense, iMax <: Dense, jMax <: Dense, i <: Dense, j <: Dense, prod <: Dense, P <: Pali] =
		p2[prod, max,
			Apply2[prod, i, j, i, j#Dec, P],
			Apply2[max, iMax, jMax, i, j#Dec, P],
			Result ]

	type Apply2[max <: Dense, iMax <: Dense, jMax <: Dense, i <: Dense, j <: Dense, P <: Pali] =
		j#Match[ Apply3[max, iMax, jMax, i, j, P], Apply3[max, iMax, jMax, i#Dec, i#Dec, P], Result]

	type Apply3[max <: Dense, iMax <: Dense, jMax <: Dense, i <: Dense, j <: Dense, P <: Pali] =
			i#Match[ P#Apply[max, iMax, jMax, i, j, P], max, Result] //Triple[max, iMax, jMax]

	type p1[D <: Dense, T <: Up, F <: Up, Up] = D#Reverse[DNil]#Compare[D]#Match[F, T, F, Up]
	type p2[D <: Dense, O <: Dense, T <: Up, F <: Up, Up] = p1[D, D#Compare[O]#Match[F, F, T, Up], F, Up]
	
	type App[start <: Dense, P <: Pali] = Apply[_1, _1, _1, start, start, P]
}
object Pali1 extends Pali0
{
	println( toInt[Pali0#App[_7, Pali0]] )
}
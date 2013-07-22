package up


import Dense._

trait Pali {
  type Result = Triple[Dense, Dense, Dense]
  type Apply[max <: Dense, iMax <: Dense, jMax <: Dense, i <: Dense, j <: Dense, P <: Pali] <: Result
}

trait Pali0 extends Pali {
  // entry point
  type App[start <: Dense, P <: Pali] = Apply[_1, _1, _1, start, start, P]

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
    i#Match[ P#Apply[max, iMax, jMax, i, j, P], Triple.Apply[max, iMax, jMax, Dense], Result]

  type p1[D <: Dense, T <: Up, F <: Up, Up] = D#Reverse[DNil]#Compare[D]#Match[F, T, F, Up]
  type p2[D <: Dense, O <: Dense, T <: Up, F <: Up, Up] = p1[D, D#Compare[O]#Match[F, F, T, Up], F, Up]
}

object Pali1 extends Pali0 {
  println( toTuple3[Pali0#App[_7, Pali0]] )
  // (21,7,3)

  // already pretty slow at _15
  //println( toTuple3[Pali1.App[_15, Pali1.type]] )
  // (195,15,13)
}

object Normal {
  case class Result(prod: Int, iMax: Int, jMax: Int)
  implicit object ResultOrdering extends Ordering[Result] {
    def compare(a: Result, b: Result) = a.prod compare b.prod
  }

  def apply(start: Int): Result = all(start).max
  def all(start: Int) : Traversable[Result] =
    for(i <- (start to 1 by -1);
      j <- (i to 1 by -1);
      val prod = i*j
      if isPalindrome(prod) )
    yield
      Result(prod, i, j)
  def isPalindrome(i: Int) = i.toBinaryString == i.toBinaryString.reverse
}

// make recursion and maximum explicit
// no custom classes
// no intermediate variables
object Explicit {
  type Result = (Int, Int, Int)
  def apply(start: Int): Result = apply( (1,1,1), start, start)

  def apply(max: Result, i: Int, j: Int): Result =
    apply1(max, (i*j, i, j))

  def apply1(max: Result, iteration: Result): Result =
    if(isPalindrome(iteration._1) && iteration._1 > max._1)
      apply2(iteration, iteration)
    else
      apply2(max, iteration)

  def apply2(max: Result, iteration: Result): Result =
    if(iteration._3 == 1)
    {
      if(iteration._2 == 1)
        max
      else
        apply(max, iteration._2 - 1, iteration._2 - 1)
    }
    else
      apply(max, iteration._2, iteration._3 - 1)

  def isPalindrome(i: Int) = i.toBinaryString == i.toBinaryString.reverse
}

// pass all arguments to functions expanded (no tuples)
// some rearranging related to 'if' to line up with functionality available
//   at type-level
object Expanded {
  type Result = (Int, Int, Int)
  def apply(start: Int): Result = apply(1, 1, 1, start, start)
  def apply(max: Int, iMax: Int, jMax: Int, i: Int, j: Int): Result =
    apply1(max, iMax, jMax, i, j, i*j)

  def apply1(max: Int, iMax: Int, jMax: Int, i: Int, j: Int, prod: Int): Result =
    p2(prod, max,
      apply2(prod, i, j, i, j - 1),
      apply2(max, iMax, jMax, i, j - 1) )

  def apply2(max: Int, iMax: Int, jMax: Int, i: Int, j: Int): Result =
    if(j > 0) apply3(max, iMax, jMax, i, j) else apply3(max, iMax, jMax, i - 1, i - 1)

  def apply3(max: Int, iMax: Int, jMax: Int, i: Int, j: Int): Result =
    if(i > 0) apply(max, iMax, jMax, i, j) else (max, iMax, jMax)

  def p2[T](prod: Int, max: Int, ifNew: => T, ifNot: => T): T =
    p1(prod, if(prod > max) ifNew else ifNot, ifNot)

  def p1[T](prod: Int, ifPalindrome: => T, ifNot: => T) =
    if( prod.toBinaryString == prod.toBinaryString.reverse ) ifPalindrome else ifNot
}

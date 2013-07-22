package up


trait Recurse {
  type Next <: Recurse
  type X[R <: Recurse] <: Int
}

trait RecurseA extends Recurse {
  type Next = RecurseA
  type X[R <: Recurse] = R#X[R#Next]
}

object Recurse {
  // uncomment to get an infinite loop
  //type C = RecurseA#X[RecurseA]
  // shows that type arguments are strictly evaluated
  //type D = Booleans.True#If[Int, RecurseA#X[RecurseA], Int]
}

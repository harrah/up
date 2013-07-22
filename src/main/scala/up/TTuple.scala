package up


sealed trait Triple[+A, +B, +C] {
    type _1 <: A
    type _2 <: B
    type _3 <: C
}
sealed trait Pair[+A, +B] {
    type _1 <: A
    type _2 <: B
}
object Triple {
    type Apply[A, B, C, Up] = Triple[Up, Up, Up] {
        type _1 = A
        type _2 = B
        type _3 = C
    }
}
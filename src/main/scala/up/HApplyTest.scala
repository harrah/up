package up


import HList._

object HApplyTest 
{
    // heterogeneous map ...
    //   We construct an HList of data and an HList of functions and apply the functions to the data cell-by-cell.

        // data HLists
    val y1 = 9.75 :: 'x' :: HNil
    val y2 = -2.125 :: 'X' :: HNil
        // the functions to apply
    val z = ((_: Double) + .5) :: ( (_: Char).isUpper) :: HNil
    // apply, check result values and types
    val z1 = happly(z)(y1)
        // check types
    val z1Types : Double :: Boolean :: HNil = z1
        // check values
    val 10.25 :: false :: HNil = z1

    val z2 = happly(z)(y2)
        // check types
    val z2Types : Double :: Boolean :: HNil = z2
        // check values
    val -1.625 :: true :: HNil = z2

        // more complicated example, with nested HLists
    val xx = List(3, 4, 5) :: (3 :: 'w' :: HNil) :: HNil
        // function to apply to the List element
    val zsub1 = (_: List[Int]).flatMap("s" * _)
        // heterogeneous function to apply to the HList element.
        //   Note the call to `part` for partial application since it isn't actually a function
    val zsub2 = partial( ( (_: Int) * 13) :: ( (_: Char).isLetter ) :: HNil )
        // combine the two functions into one HList
    val f = zsub1 :: zsub2 :: HNil
        // do the heterogeneous map- note that no types need to be explicitly provided.
    val r = happly(f)(xx)

        // types
    val rTypes : List[Char] :: ( Int :: Boolean :: HNil ) :: HNil = r
        // values
    val ExpectedHead = List.fill(xx.head.sum)('s')
    val ExpectedHead :: (39 :: true :: HNil) :: HNil = r

    def run()
    {
        println(z1)
        println(z2)
        println(r)
    }
}
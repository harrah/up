import HList._
import Nat._

object HListTest
{
	// a heterogeneous list of length 3 and type
	//  Int :: String :: List[Char] :: HNil
	val a = 3 :: "ai4" :: List('r','H') :: HNil
	// a heterogeneous list of length 4 and type
	//  Char :: Int :: Char :: String :: HNil
	val b = '3' :: 2 :: 'j' :: "sdfh" :: HNil
	// the two HLists zipped
	val c = a zip b
	// zipped again.
	val cc = c zip c.tail

	// verify proper types
	// note that the fourth element of b is dropped, like when zipping a homogeneous List
	val checkCType : (Int, Char) :: (String, Int) :: (List[Char], Char) :: HNil = c
	val checkCCType : ((Int, Char), (String, Int)) :: ((String, Int), (List[Char], Char)) :: HNil = cc

	// verify proper values
	val (3, '3') :: ("ai4", 2) :: (List('r', 'H'), 'j') :: HNil = c
	val ((3,'3'),("ai4",2)) :: (("ai4",2),(List('r', 'H'),'j')) :: HNil = cc

	// unzip the zipped HLists
	val (cc1, cc2) = unzip(cc)
	val (ca, cb) = unzip(cc1)
	// check types
	val checkCC1 : (Int, Char) :: (String, Int) :: HNil = cc1
	val checkCC2 : (String, Int) :: (List[Char], Char) :: HNil = cc2
	val checkCa: Int :: String :: HNil= ca
	val checkCb: Char :: Int :: HNil = cb

	// Moving along to a heterogeneous map ...
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

		// append, which is implemented as a foldr
	val ab = a ::: b
		// types
	val checkAB : Int :: String :: List[Char] :: Char :: Int :: Char :: String :: HNil = ab
		// values
	val 3 :: "ai4" :: List('r','H') :: '3' :: 2 :: 'j' :: "sdfh" :: HNil = ab

		// length of an HList, on type and value levels
	val 7 = ab.length
	implicitly[_7 =:= ab.Length]

		// reverse
	val reversed = b.reverse
		// types
	val checkReversed : String :: Char :: Int :: Char :: HNil = reversed
		// values
	val "sdfh" :: 'j' :: 2 :: '3'  :: HNil = reversed

		// last
	val last = reversed.last
		// type
	val checkLast : Char = last
		// value
	val '3' = last

		// reverse_:::
	val reverseAppend = a reverse_::: b
		// types
	val checkReverseAppend : List[Char] :: String :: Int :: Char :: Int :: Char :: String :: HNil = reverseAppend
		// values
	val  List('r','H') :: "ai4" :: 3 :: '3' :: 2 :: 'j' :: "sdfh" :: HNil = reverseAppend

		// Index related functions
	val x = 3 :: true :: "asfd" :: false :: 'k' :: () :: 13 :: 9.3 ::  HNil
	val b2: Boolean = x.i[_3].at
	val pre: Boolean = x.i[_3].drop.i[_0].at
	val rep = x.i[_3].replace(19)
	val mp = x.i[_4].map(_.isLower)
	val rm = x.i[_5].remove
	val fmp = x.i[_2].flatMap( s => s.charAt(0) :: s.substring(1) :: HNil )
	val ins0 = x.i[_0].insert(List(3,4))
	val ins7 = x.i[_7].insert(-3.0f)
	val insH = rm.i[_3].insertH( 'h' :: b2 :: Some(3) :: None :: HNil )
	val (aa, bb) = ins7.i[_6].splitAt

	val dropRight = x.reverse.i[_3].drop.reverse

	def run()
	{
		println(c)
		println(cc)
		println(z1)
		println(z2)
		println(r)
		println(ab)
		println(reversed)
		println(last)
		println(reverseAppend)

		println()
		println(b2)
		println(rm)
		println(fmp)
	}
}
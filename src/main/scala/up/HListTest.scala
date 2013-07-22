package up


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
	
	val app1 = (3 :: true :: 'c' :: HNil)( i => b => c => if(b) i else c.toInt )
	val app2 = (3 :: false :: 'c' :: HNil)( i => b => c => if(b) i else c.toInt )

	val tipX = 3 :: "asdf" :: HNil
	// want to do this:
//	val tipH = tipX.t[Int].at
	//   but have to do this because of #3201
	val tipH = tipToInd(tipX.t[Int]).at


	def run()
	{
		println(c)
		println(cc)
		println(ab)
		println(reversed)
		println(last)
		println(reverseAppend)

		println()
		println(b2)
		println(rm)
		println(fmp)

		println()
		println(app1)
		println(app2)
	}
}

final class MyTypeMap extends TypeMap
{
	// define allowed mappings
	implicit val StrToBool = new TypeMapping[String, Boolean] {}
	implicit val StrToInt = new TypeMapping[String, Int] {}
	implicit val IntToStr = new TypeMapping[Int, String] {}
	implicit val LStrToStr = new TypeMapping[List[String], Int] {}
	implicit val LIntToStr = new TypeMapping[List[Int], Int] {}
}

object TypedMapTest
{
	val m = new MyTypeMap
	import m._

	m("a") = 3
	m("a") = true
		// compilation error
	//m("a") = "string"

	m(5) = "asdf"
	m(17) = "jkl"


	m( List[String]() ) = 9
	m( List[Int]() ) = 4
	m( 5 :: Nil ) = 7
	m( "xt" :: Nil ) = 7
		// compilation error
	//m( 'z' :: Nil) = 0

	def run()
	{
		assert( m[String, Int]("a") == 3 )
		assert( m[String, Boolean]("a") == true )

		assert( m.getOr("a", 5) == 3)
		assert( m.getOr("b", 5) == 5)
		assert( m.getOr("a", false) == true)
		assert( m.getOr("b", false) == false)
	}
}
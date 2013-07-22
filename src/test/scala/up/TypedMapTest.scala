package up

import org.specs2.Specification

final class MyTypeMap extends TypeMap {
  // define allowed mappings
  implicit val StrToBool = new TypeMapping[String, Boolean] {}
  implicit val StrToInt = new TypeMapping[String, Int] {}
  implicit val IntToStr = new TypeMapping[Int, String] {}
  implicit val LStrToStr = new TypeMapping[List[String], Int] {}
  implicit val LIntToStr = new TypeMapping[List[Int], Int] {}
}

class TypedMapTest extends Specification {
  def is = "TypedMap should" ^
    "work as expected" ! run ^
                         end

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

  def run = {
    m[String, Int]("a") must_== 3
    m[String, Boolean]("a") must_== true
    m(5) must_== "asdf"
    m(17) must_== "jkl"
    m( List[String]() ) must_== 9
    m( List[Int]() ) must_== 4
    m( 5 :: Nil ) must_== 7
    m( "xt" :: Nil ) must_== 7

    m.getOr("a", 5) must_== 3
    m.getOr("b", 5) must_== 5
    m.getOr("a", false) must_== true
    m.getOr("b", false) must_== false
  }
}

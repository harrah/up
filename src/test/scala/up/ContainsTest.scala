package up

import org.specs2.Specification

import HList._
import Contains._

class ContainsTest extends Specification {
  def is = "Contains should" ^
    "work as expected" ! run ^
                         end

  def run = {
    mustContain[Int, Int :: Double :: Boolean :: HNil]
    mustContain[Int, Double :: Boolean :: Int :: HNil]
    mustNotContain[Int, Double :: Boolean :: HNil]

    excludeDemo("works")
    excludeDemo(3.0f)
    excludeDemo( () )

    // The following should fail
    /*mustNotContain[Int, Int :: Double :: Boolean :: HNil]
     mustNotContain[Int, Double :: Boolean :: Int :: HNil]
     mustContain[Int, Double :: Boolean :: HNil]

     excludeDemo(3)
     excludeDemo(false)
     excludeDemo(9.3)*/
    ok
  }
}

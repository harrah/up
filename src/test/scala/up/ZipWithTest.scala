package up

import org.specs2.Specification
import ZippedK._
import KList._
import HList.{:: => :+: }
import ~>._

class ZipWithTest extends Specification {
  def is = "ZipWith should"  ^
    "work as expected" ! run ^
                         end

  def run = {
    val a = Stream(1,2,5,3,9,10,101)
    val b = Stream("as", "tu", "kp", "lja")
    val c = Stream(true, false, false, true, true)

    val f = (a: Int, b: String, c: Boolean) => if(c) a + b.length else b.sum

    val d0 = ZipWith.zipWith(a :^: b :^: c :^: KNil) {
      case a :+: b :+: c :+: HNil =>
        if(c) a + b.length else b.sum
    }
    println(d0.toList)

    val d = (a,b,c) zipWith f
    println(d.toList)

    val g = (a: Int, b: String, c: Boolean) => c && b.length == a
    val d2 = (a,b,c) forall g
    println(d2)

    val d3 = (a,b,c) exists g
    println(d3)
    println()

    val prnt = (a: Int, b: String, c: Boolean) => println( (a,b,c) )
    (a,b,c) foreach prnt

    println("\n=====")
    val fc = (a: Int, b: String, c: Boolean) => {println( (a,b,c) ); if(c) Some(b + a) else None }
    val d4 = (a,b,c) collect fc
    d4.foreach { _ => println() }
    println("=====")

    val a2 = Stream.from(0)
    val b2 = Stream.continually(3)
    val c2 = Stream.iterate(2)(_ * 2)
    val fm = (a: Int, b: Int, c: Int) => Stream.range(a, c)
    val d5 = (a2,b2,c2) flatMap fm
    println(d5.take(100).toList)

    ok
  }
}

import org.scalatest._
import Arithmetic._

class ArithmeticTest extends FlatSpec {
  "defs of Arithmetic" should "be Ok" in {
    val s = new S99Int(10)
    implicit def intToS99Int(x: Int) = new S99Int(x)
    assert(s.gcd(25,10) == 5)
    assert(s.gcd(36, 63) == 9)
    assert(35.isCoprimeTo(64))
    assert(10.totient == 4)
    assert(315.primeFactors == List(3,5,7))
    assert(28.golbach == (5,23))
  }
}

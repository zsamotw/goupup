
/**
  * implementations of exercises of ninety nine scala problems from http://aperiodic.net/phil/scala/s-99/
  */

object Arithmetic {

  implicit def intToS99Int(x: Int) = new S99Int(x)

  class S99Int(val x: Int) {

    //31
    def isPrime: Boolean = {
      val dividers = 2 to x / 2 
      dividers.forall(x % _ != 0)
    }

    //32
    def gcd(a: Int, b: Int): Int = {
      if(a % b == 0) b else gcd(b, a % b)
    }

    //33
    def isCoprimeTo(y: Int) = gcd(x,y) == 1

    //34
    def totient = ( 1 to x filter(i => i.isCoprimeTo(x)) ).length
      //( for(i <- 1 to x; if(i.isCoprimeTo(x))) yield i ).toList.length
    //35
    def primeFactors = {
      val factors = 2 to x / 2 filter(x % _ == 0)
      factors.toList.collect{case f if(f.isPrime) =>  f}
    }
  }

}

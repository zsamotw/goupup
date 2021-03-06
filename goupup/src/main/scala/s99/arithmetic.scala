
/**
  * implementations of exercises of ninety nine scala problems from http://aperiodic.net/phil/scala/s-99/
  */

object Arithmetic {

  implicit def intToS99Int(x: Int) = new S99Int(x)

  class S99Int(val x: Int) {

    //31
    def isPrime: Boolean = 2 to x / 2 forall(x % _ != 0)

    //32
    def gcd(a: Int, b: Int): Int = {
      if(a % b == 0) b else gcd(b, a % b)
    }

    //33
    def isCoprimeTo(y: Int) = gcd(x,y) == 1

    //34
    def totient = ( 1 to x filter(i => i.isCoprimeTo(x)) ).length

    //35
    def primeFactors = {
      val factors = 2 to x / 2 filter(x % _ == 0)
      factors.toList.collect{case f if(f isPrime) =>  f}
    }

    //40
    def golbach = {
      val primes = 2 to x filter(_ isPrime)
      val res = for{
        p1 <- primes
        p2 <- primes
        if(p1 != p2 &&
             p1 + p2 == x)
      } yield (p1, p2)
      res.head
    }
  }

  /*
   *Euler project
   */

  //If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
  //Find the sum of all the multiples of 3 or 5 below 1000.
  def sumOfMultiples = (1 until 1000) filter(i => (i % 3 == 0 || i % 5 == 0)) sum

//  Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:
//  1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
//  By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
  val euler2 = {
    def fib: Stream[Int] = 1 #::   fib.scanLeft(0)(_+_)
    fib takeWhile(_ < 4000000) filter(_ % 2 == 0) sum
  }
  //The prime factors of 13195 are 5, 7, 13 and 29.
  //What is the largest prime factor of the number 600851475143 ?
  def largerPrimeFactor(x: Int) = {
    ( 2 to x/2 ).filter(i => (i.isPrime && x % i == 0)).reduceLeft((a,b) => if (a>b) a else b)
  }
  //A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
  //Find the largest palindrome made from the product of two 3-digit numbers.
  def largestPalindrome = (10 to 99).flatMap(d1 => 10 to 99 map(d2 => d1 * d2)).filter(el => el.toString == el.toString.reverse).max

  //2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
  //What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
  def smallestDivided = {
    val divs = 1 to 20
    ( for(i <- 1 to Int.MaxValue; if(divs.forall(i % _ == 0))) yield i ).head
  }
}

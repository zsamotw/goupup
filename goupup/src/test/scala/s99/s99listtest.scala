import org.scalatest._
import Lists._ 

class ListTest extends FlatSpec {

  "defs of lists" should "be ok" in {
    val xs = List(1,2,3,4,5)
    assert(last(xs) == 5 )
    assert(nth(2, xs) == 3)
    assert(length(xs) == 5)
    assert(reverse(xs) == List(5,4,3,2,1))
    assert(isPalindrome(List(1,2,3,2,1)) == true)
    assert(isPalindrome(xs) == false)
    val ys = List(1,1,4,4,4,5)
    assert(compress(ys) == List(1,4,5))
    assert(pack(ys) == List(List(1,1), List(4,4,4), List(5)))
    assert(encode(ys) == List((1,2), (4,3), (5,1)))
    assert(encodeDirectly(ys) == List((1,2), (4,3), (5,1)))
    val zs = List((1,3),(2,1), (3,2), (1,1))
    assert(decode(zs) == List(1,1,1,2,3,3,1))
    assert(duplicate(List(1,2,3)) == List(1,1,2,2,3,3))
    assert(duplicateN(List(1,2,3),3) == List(1,1,1,2,2,2,3,3,3))
  }
}


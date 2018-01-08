import org.scalatest.FunSuite

class CesarTest extends FunSuite {
  val c = new cesar.Cesar(88)

  test("check cesar code") {
    val afterCode = c.encode("abcdfghijklmnopqwrz")
    val afterENcode = c.decode(afterCode)
    assert( "abcdfghijklmnopqwrz" == afterENcode)
  }

  test("check set code char") {
    val ch = c.setCode('~', 139)
    assert(ch == 42)
  }

  test("check set code char2") {
    val ch1 = c.setCode('a', 72)
//    println("a + 72: " + ch1)
    val res = c.getCharCode(ch1.toChar,72)
    assert(res == 97)
  }

  test("check get car code") {
    val ch = c.getCharCode('!', 10)
    // println('!'.toInt - 10)
    assert(ch == 119)
  }

  test("check get char3") {
    val ch = c.getCharCode('!', 34)
    assert(ch == 95)
  }
}

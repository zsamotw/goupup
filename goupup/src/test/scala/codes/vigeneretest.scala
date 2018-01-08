import org.scalatest.FunSuite

class VigenereTest extends FunSuite {
  val v = new vigenere.Vigenere()
  test("test shift method") {
    val s = v.shift(121, 1)
    assert(s == 122)
  }

  // test("print sqr") {
  //   val s = v.codeSqr
  //   println(s.toString)
  // }

  test("check key and fraze") {
    val fraze = "abcde"
    val key = "abcdef"
    val res = v.setKey(fraze, key)
    assert(res == "abcde")
  }

  test("check encode char") {
    val ch = v.encodeChar('f', 'g', v.codeSqr)
    assert(ch == 'l')
  }

  test("check decode char") {
    val ch = v.decodeChar('l', 'g', v.codeSqr)
    assert(ch == 'f')
  }

  test("check encode decode") {
    val encodeF = v.encode("hallo", "ghu")
    println("encoded: " + encodeF)
    val decodeF = v.decode(encodeF, "ghu")
    assert("hallo" == decodeF)
  }
}

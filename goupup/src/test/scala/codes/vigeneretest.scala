import org.scalatest.FunSuite

class VigenereTest extends FunSuite {
  val v = new vigenere.Vigenere()
  test("test shift method") {
    val s = v.shift(121, 1)
    assert(s == 122)
  }

  test("print sqr") {
    val s = v.codeSqr
    println(s.toString)
  }

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
}

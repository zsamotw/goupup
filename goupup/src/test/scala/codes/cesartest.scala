import org.scalatest.FunSuite

class CesarTest extends FunSuite {
  test("check cesar code") {
    val c = new cesar.Cesar("tomasz", 10)
    val afterCode = c.encode
    val afterENcode = c.decode(afterCode)
    assert(c.fraze == afterENcode)
  }
}

package cesar

class Cesar(val shift: Int) {

  val maxBound: Int = 127
  val minBound: Int = 32

  def encode(fraze: String) = {
    val ll = fraze map(el => setCode(el, shift))
    ll.toList.foldLeft("")((acc,el) => acc + el.toChar)
  }

  def decode(str: String) = {
    val ll = str map(el => getCharCode(el, shift))
    ll.toList.foldLeft("")((acc, el) => acc + (el.toInt).toChar)
  }

  def setCode(ch: Char, shift: Int): Int = {
    val n = ch.toInt + shift
    n match {
      case _ if(n > maxBound) => n % maxBound + minBound - 1
      case _ => n
    }
  }

  def getCharCode(ch: Char, shift: Int): Int = {
    val n = ch.toInt - shift
    n match {
      case _ if(n < 0) => maxBound - ((Math.abs(n) + minBound) % maxBound) + 1
      case _ if(n < minBound) => maxBound - (minBound - n) + 1
      case _ => n
    }
  }
}

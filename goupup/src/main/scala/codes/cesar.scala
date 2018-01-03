package cesar

class Cesar(val fraze: String, val shift: Int) {
  def encode = {
    val ll = fraze map(el => el.toInt + shift)
    ll.toList.foldLeft("")((acc,el) => acc + el.toChar)
  }
  def decode(str: String) = {
    val ll = str map(el => ( el.toInt - shift ).toChar)
    ll.toList.foldLeft("")((acc, el) => acc + (el.toInt).toChar)
  }
}

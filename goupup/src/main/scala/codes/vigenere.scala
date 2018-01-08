package vigenere 

class Vigenere {

  /**
    * final values
    **/

  val minBound = 97
  val maxBound = 122
  val firstListIndex = 1

  /**
    * Methods for creating code squere
    **/

  def shift(i: Int, shift: Int) = {
    val j = i + shift
    j  match {
      case _ if(j > maxBound) => j % maxBound + minBound - 1
      case _ => j
    }
  }

  val withoutShift: List[List[Int]] = {
    val seq1 = {
      for(i <- 0 to 25) yield {
        val seq2 = for(j <- 97 to 122) yield j
        seq2.toList
      }
    }
    seq1.toList
  }

  def mapToShifted(sqr: List[List[Int]], sh: Int): List[List[Int]] = {
    sqr match {
      case Nil => Nil
      case l :: rest => l.map{el => shift(el, sh)} :: mapToShifted(rest, sh + 1)
    }
  }

  val codeSqr = mapToShifted(withoutShift, 0)

  /**
    * Methods for setting code fraze
    **/

  def setKey(fraze: String, key: String): String = {
    (fraze.length(), key.length()) match {
      case(fs, ks) if(ks > fs) => key.take(fs)
      case (fs, ks) if(ks < fs) => {
        val factor = fs / ks
        val rest = fs % ks
        concatKey(key, factor, rest)
      }
      case _ => key
    }
  }

  def concatKey(key: String, factor: Int, rest: Int): String = {
    factor match {
      case 0 => key.take(rest)
      case _ => key + concatKey(key, factor - 1, rest)
    }
  }

  /**
    * Methods for encoding / decoding
    **/

  def encodeChar(chFromFraze: Char, charFromKey: Char, sqr: List[List[Int]]): Char = {
    val fromFrazeInt = chFromFraze.toInt
    val fromKeyInt = charFromKey.toInt

    val horizontalIndex = sqr.take(firstListIndex).flatten.indexOf(fromFrazeInt)
    val listToWork = sqr.flatMap(list => if(list.head == fromKeyInt) list else Nil)
    listToWork(horizontalIndex).toChar
  }

  def decodeChar(chFromFraze: Char, charFromKey: Char, sqr: List[List[Int]]): Char = {
    val fromFrazeInt =chFromFraze.toInt
    val fromKeyInt = charFromKey.toInt

    val listWithKeyVal = sqr.flatMap(list => if(list.head == fromKeyInt) list else Nil)
    val horizontalIndex = listWithKeyVal.indexOf(fromFrazeInt)
    val intVal = sqr.take(firstListIndex).flatten
    intVal(horizontalIndex).toChar
  }

  def encode(fraze: String, codeKey: String): String = {
    val equalCode = setKey(fraze, codeKey)
    val tuples = fraze zip equalCode
    (tuples map{case (f,c) => encodeChar(f, c, codeSqr)}).mkString
  }

  def decode(fraze: String, codeKey: String): String = {
    val equalCode = setKey(fraze, codeKey)
    val tuples = fraze zip equalCode
    (tuples map{case (f,c) => decodeChar(f, c, codeSqr)}).mkString
  }
}

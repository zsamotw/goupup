package huffman
object Huffman {

  abstract class CodeTree

  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  def weight(tree: CodeTree) = {
    tree match {
      case Fork(_, _, _, w) => w
      case Leaf(_, w) => w
    }
  }

  def chars(tree: CodeTree) = {
    tree match {
      case Fork(_, _, ch, _) => ch
      case Leaf(ch, _) => List(ch)
    }
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) = {
    Fork(left, right, chars(left)::: chars(right), weight(left) + weight(right))
  }

  def string2Chars(str: String) = str.toList

  def times(chars: List[Char]): List[(Char, Int)] = {
    def tuplesCreator(tuples: List[(Char, Int)], ch: Char): List[(Char, Int)] = {
      val contains = tuples.foldRight(false)((el, acc) => if(el._1 == ch) true || acc else false || acc)
      if(contains) tuples map{case (char, w) => if ( char == ch ) (char, w + 1) else (char,w)}
      else (ch,1) :: tuples
    }
    chars.foldLeft(List[(Char, Int)]())((acc, el) => tuplesCreator(acc, el))
  }

  def makeOrderedLeafList(freq: List[(Char, Int)]): List[Leaf] = {
    freq.map{case(char, weight) => Leaf(char, weight)}.sortWith((el1,el2) => el1.char < el2.char)
  }

  def singleton(trees: List[CodeTree]) = trees.size == 1

  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case left :: right :: rest => makeCodeTree(left, right) :: rest
    case _ => trees
  }

  def until(pre: List[CodeTree] => Boolean, factory: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
    if(pre(trees)) trees else until(pre, factory)(factory(trees))
  }

  def createCodeTree(chars: List[Char]): CodeTree = {
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head
  }


  /*
   *
   */

  type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    null
  }

  def encode(tree: CodeTree, chars: List[Char]): List[Bit] = {
    null
  }

}

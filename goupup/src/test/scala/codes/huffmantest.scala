import org.scalatest.FlatSpec
import huffman.Huffman._

class HuffmanTest extends FlatSpec  {
  it should "list after def times should looks like this" in {
    val chars = string2Chars("hallo")
    val res = times(chars)
    // println(res)
    assert(res.sorted == (List(('h', 1), ('a', 1), ('l', 2), ('o', 1))).sorted)
  }

  it should "proper Leafs list " in {
    val chars = List(('h', 1), ('a', 1), ('l', 2))
    val leafs =makeOrderedLeafList(chars)
    assert( leafs == List(Leaf('a',1), Leaf('h', 1), Leaf('l', 2)) )
  }

  it should "make code tree" in {
    val ctl = until(singleton, combine)(List(Leaf('h',1), Leaf('a', 1), Leaf('l', 2)))
    val ctlbis = createCodeTree(List('a', 'l', 'a', 'm', 'a'))
    // println(ctl)
    // println(ctlbis)
  }


  
}

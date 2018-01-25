/**
  * implementations of exercises of ninety nine scala problems from http://aperiodic.net/phil/scala/s-99/
  */

trait Tree[+T] {
  //57
  def addValue[B >: T](value: B)(implicit ordering: Ordering[B]): Tree[B]

}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = s"{$value | ${left.toString} + ${right.toString}}"

  //57
  def addValue[B >: T](value: B)(implicit ordering: Ordering[B]): Tree[B] = {
    import ordering._
    if (value < this.value)  Node(this.value, this.left.addValue(value), this.right)
    else if (value > this.value)  Node(this.value, this.left, this.right.addValue(value))
    else  this
  }
}

case object End extends Tree[Nothing] {
  override def toString = "."

  //57
  def addValue[B >: Nothing](value: B)(implicit ordering: Ordering[B]): Tree[B] = Node(value, End, End)
}

object MyTree {
  //55
  def balanced[T](n: Int, value: T): Tree[T] = {
    n match {
      case 0 => End
      case 1 => Node(value, balanced(n - 1, value), End)
      case i if i >= 2 => Node(value, balanced(n - 1, value), balanced(n - 2, value))
    }
  }

  //56
  def isSymetric[T](tree: Tree[T]): Boolean = {
    def isMirror(tree1: Tree[T], tree2: Tree[T]): Boolean = (tree1, tree2) match {
      case (End, End) => true
      case (node1: Node[T], node2: Node[T]) => isMirror(node1.left, node2.left) && isMirror(node1.right, node2.right)
      case _ => false
    }
    tree match {
      case End => false
      case Node(value: T, left: Tree[T], right: Tree[T]) => isMirror(left, right)
    }
  }

  //58
//   def symetricBalancedTree[T](n: Int, value: T): Tree[T] = {
//     n match {
//       case 1 => Node(value, End, End)
// //      case 2=> End
// //      case 3 =>  Node(value, symetricBalancedTree(n - 2, value), End)
//       case n if n >= 3 => Node(value, symetricBalancedTree(n - 2, value), symetricBalancedTree(n - 2, value))
//     }
//   }

  //59
  def highBalTree[T](n: Int, value: T): Tree[T] = {
    n match {
      case 1 =>  Node(value, End, End)
      case n if n > 1 => Node(value, highBalTree(n - 1, value), highBalTree(n - 1, value))
    }

  }
}


/**
  * implementations of exercises of ninety nine scala problems from http://aperiodic.net/phil/scala/s-99/
  */


object Trees {
  abstract class Tree[+T] {

    //55
    def balanced[T](n: Int, value: T): Tree[T] = {
      n match {
        case 0 => End
        case 1 => Node(value, balanced(n - 1, value), End)
        case i if i >= 2 => Node(value, balanced(n - 2, value), balanced(n - 2, value))
      }
    }
  }

  //56
  def isSymetric[T](tree: Tree[T]) = {
    def isMirror(tree1: Tree[T], tree2: Tree[T]): Boolean = (tree1, tree2) match {
      case (End, End) => true
      case (node1: Node[T], node2: Node[T]) => isMirror(node1.left, node2.left) && isMirror(node1.right, node2.right)
      case _ => false
    }
    tree match {
      case End => false
      case Node(_: T, left: Tree[T], right: Tree[T]) => isMirror(left, right)
    }
  }

  //57
  def addValue[U >: T <% Ordered[T]](value: U, tree: Tree[U]) = {

  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree {
    override def toString = s"{$value | ${left.toString} + ${right.toString}}"
  }

  case object End extends Tree[Nothing] {
    override def toString = "."
  }
  
  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }
}
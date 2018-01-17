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

    //56
    def isSymetric[T] = {
      def isMirror(tree1: Tree[T], tree2: Tree[T]): Boolean = (tree1, tree2) match {
        case (End, End) => true
        case (node1: Node[T], node2: Node[T]) => isMirror(node1.left, node2.left) && isMirror(node1.right, node2.right)
        case _ => false
      }
      this match {
        case End => false
        case Node(_: T, left: Tree[T], right: Tree[T]) => isMirror(left, right)
      }
    }

    //57
    def addValue[B >: T](value: B)(implicit less: (B, B) => Boolean):Tree[B]


  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree {
    override def toString = s"{$value | ${left.toString} + ${right.toString}}"

    //57
    override def addValue[B >: T](value: B)(implicit less: (B, B) => Boolean): Tree[B] = this.value match {
      case v:T if !less(v,value) => Node(v, this.left.addValue(value), this.right)
      case v:T if less(v, value) => Node(v, this.left, this.right.addValue(value))
      case _ => this
    }
  }

  case object End extends Tree[Nothing] {
    override def toString = "."

    //57
    override def addValue[B >: Nothing](value: B)(less: (B,B) => Boolean): Tree[B] = Node(value, End, End)
  }
}
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
  def balanced[T](n: Int, value: T): List[Tree[T]] = {
    n match {
      case 0 => Nil
      case 1 => List(Node(value, End, End))
      case 2 => List(Node(value, Node(value, End, End), End), Node(value, End, Node(value, End, End)))
      case _ =>
        if(n % 2 == 0) {
          (for{
             l <- balanced(n/2 - 1, value)
             r <- balanced(n/2, value)
           } yield Node(value, l, r)) ++
            (for{
               l <- balanced(n/2, value)
               r <- balanced(n/2 - 1, value)
             } yield Node(value, l, r))
        }
        else {
          for{
            l <- balanced((n - 1) / 2, value)
            r <- balanced((n -1) /  2, value)
          } yield Node(value, l, r)
        }
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


  //59
  def highBalTree[T](n: Int, value: T): Tree[T] = {
    n match {
      case 1 =>  Node(value, End, End)
      case n if n > 1 => Node(value, highBalTree(n - 1, value), highBalTree(n - 1, value))
    }

  }

  //61
  def leafCount[T](tree: Tree[T]): Int = {
    tree match {
      case End => 0
      case Node(_, left, right) => 1 + leafCount(left) + leafCount(right)
    }
  }

  //61A 
  def leafList[T](tree: Tree[T]): List[T] = {
    tree match {
      case End => Nil
      case Node(value, left, right) => value :: leafList(left) ++ leafList(right)
    }
  }

  //62
  def internalList[T](tree: Tree[T]): List[T] = {
    tree match {
      case End => Nil
      case Node(_, End, End) => Nil
      case Node(value, left, right) => value :: internalList(left) ++ internalList(right)
    }
  }

  //62A
  def atLevel[T](n: Int, tree: Tree[T]): List[T] = {
    n match {
      case 1 =>
        tree match {
          case End => Nil
          case Node(value, _, _) => List(value)
        }
      case _ =>
        tree match {
          case End => Nil
          case Node(value, left, right) => atLevel(n - 1, left) ++ atLevel(n -1, right)
        }

    }
  }
}


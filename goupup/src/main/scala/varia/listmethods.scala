package varia

object listMethods {

  val list = List(1,3,3,4,2,3,4,2,5,6)

  def pack(xs: List[Int]): List[List[Int]] = {
    xs match {
      case Nil => Nil
      case x :: xs1 => {
        val(first, rest) = xs partition(y => y == x)
        first :: pack(rest)
      }
    }
  }

  def encode(xs: List[Int]): List[List[(Int, Int)]] = {

    xs match {
      case Nil => Nil
      case x :: xs1 => {
        pack(xs) match {
          case y :: ys => List((y.head, y.length)) :: encode(ys.flatten)
        }
      }
    }
  }

  def foldLeft[T](list: List[T])(n: T)(op: (T,T) => T): T = {
    list match {
      case Nil => n
      case x :: xs1 => foldLeft(xs1)(op(x, xs1.head))(op)
    }
  }


  def drop[T](list: List[T])(n: Int): List[T] = {
    list match {
      case Nil => Nil
      case x :: xs => {
        if(n > 0) drop(xs)(n -1)
        else xs
      }
    }
  }

  def isSubesquence[A](xs: List[A], ys: List[A]): Boolean = {
    xs match {
      case Nil => false
      case x :: xs1 =>
        if((xs take ys.size) == ys) true
        else isSubesquence(xs1, ys)
    }
  }

  case class MyList(list: List[Int]) {
    def max = list.foldLeft(list.head)((x,y) => if (x >= y) x else y)
    def product = list.foldLeft(1)((x,y) => x * y)
    def head = list.foldLeft(list.head)((x,y) => x)
    def last = list.foldLeft(list.head)((x,y) => y)
    def reverse = list.foldLeft(List[Int]())((x,y) => y :: x)
    def filter(p: Int => Boolean) = list.foldLeft(List[Int]())((x,y) => if(p(y)) y :: x else x)
    def contain(n: Int) = list.foldLeft(false)((x,y) => if(y == n) true else x)
  }

  def subsequence[A](xs: List[Option[A]]): Option[List[A]] = {
    xs match {
      case Nil => None
      case x :: xs1 =>
        Some{
          x.get :: subsequence(xs1).get
        }
    }
  }

  def count(xs: List[Int]): Int = {
    xs match {
      case Nil => 0
      case x :: xs1 => x + count(xs1)
    }
  }

  def perm(xs: List[Int]): List[List[Int]] = {
    xs.foldRight(List[List[Int]](Nil)){(x, acc) =>
      acc ++ (for{a <- acc; x <- xs} yield x :: a)
    }
  }
}

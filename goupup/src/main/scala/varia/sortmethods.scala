
object sorts {

  def isort(list: List[Int]): List[Int] = {
    def insert(x: Int, xs: List[Int]): List[Int] = {
      xs match {
        case Nil => List(x)
        case y :: ys => {
          if(x <= y) x :: xs
          else y :: insert(x, ys)
        }
      }
    }

    list match {
      case Nil => Nil
      case x :: xs => insert(x, isort(xs))
    }
  }

  def msort[T](list: List[T])(less: (T,T) => Boolean): List[T] = {
    def merge(xs: List[T], ys: List[T]): List[T] = {
      (xs,ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x :: xs1, y :: ys1) => {
          if(less(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
        }
      }
    }

    val middle = list.length / 2
    middle match {
      case 0 => list
      case _ => {
        val(first, second) = list.splitAt(middle)
        merge(msort(first)(less), msort(second)(less))
      }
    }
  }


  def numLess(x: Int, y: Int) = x <= y 
  def litLess(x: String, y: String) = x.compareTo(y) <= 0

  val lit = List("Mateusz", "Alojzy", "Dupa")
  val list = List(3,5,1,9,5,2)


  msort(list)(numLess)
  msort(lit)(litLess)

  def binnarySearch(xs:List[Int], key: Int): Int = {
    val mid = xs.length / 2
    mid match {
      case 0 => -1
      case _ => {
        val midElem = xs(mid)
        if (midElem == key) midElem
        else if(midElem > key) {
          binnarySearch(xs take mid, key)
        }
        else binnarySearch(xs drop mid, key)
      }
    }
  }

  def quickSort[T](xs: List[T])(less:(T,T) => Boolean): List[T] = {
    xs match {
      case Nil => Nil
      case (head :: tail) =>
      quickSort(tail.filter(x => less(x,head)))(less) ::: List(head) ::: quickSort(tail filter(x => !less(x, head)))(less)
    }
  }

  def bubbleSort(list: List[Int]): List[Any] = {
    def loop(xs: List[Int], n: Int, l: Int): List[Any] = {
      val xsLength = xs.length
      (n, l) match {
      case (0, _) => xs
      case (n, l) =>
          if(l == xsLength) loop(xs, n - 1, 1)
          else if(l == xsLength - 1) {
            val(y::ys, z) = xs.splitAt(l)
            val res = if(ys.last > z.head) y :: ys ::: List(z.head) else xs    //     (1,2,3) ()
            loop(res, n - 1, 1)
          }
          else if(l == 1) {
            val(y, z::zs) = xs.splitAt(l)
            val res = if(y.head > z) z :: y.head :: zs else xs
            loop(res, n, l + 1)
          }
          else {
            val(y::ys, z::zs) = xs.splitAt(l)
            val res = if(ys.last > z) y :: ys.init ::: List(z) ::: List(ys.last) ::: zs else xs
            loop(res, n, l + 1)
          }
      }
    }
    list match {
      case Nil => Nil
      case List(x) => List(x)
      case x :: y :: Nil => if(x > y) List(y, x) else List(x, y)
      case _ =>  loop(list, list.length, 1)
    }
  }
}

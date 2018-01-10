/**
  * implementations of exercises of ninety nine scala problems from http://aperiodic.net/phil/scala/s-99/
  */


object Lists { 
  //1
  def last[T](list: List[T]): T = list match {
    case (el::Nil) => el
    case (el :: rest) => last(rest)
    case _ => throw new Exception("Empty list")
  }

  //3
  def nth[T](n: Int, list: List[T]): T = {
    if(list.length - n < 0) throw new Error("No Such element")
    else {
      n match {
        case 0 => list.head
        case _ => nth(n - 1, list.tail)
      }
    }
  }

  //4
  def length[T](list: List[T]): Int = list match {
    case Nil => 0
    case x :: xs => 1 + length(xs)
  }

  //5
  def reverse[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case x :: xs => reverse(xs) ::: List(x)
  }

  //6
  def isPalindrome[T](list: List[T]): Boolean = {
    list == reverse(list)
  }

  //7
  // def flatten[T,B <: T](list: List[T]): List[B] = list match {
    // case Nil => Nil
    // case x :: xs => 
      // x match {
        // case el: T => el :: flatten(xs)
        // case _ => flatten(x) ::: flatten(xs)
      // }
  // }

  //8
  def compress[T](list: List[T]): List[T] = {
    def makeList(list: List[T], res: List[T]): List[T] = (list, res) match {
      case (Nil, res) => res
      case (x :: xs, res) =>
        if(res contains x) makeList(xs, res) else makeList(xs, res ::: List(x))
    }
    makeList(list, Nil)
  }

  //9
  def pack[T](list: List[T]): List[List[T]] = {
    def packer(result: List[List[T]], remains: List[T]): List[List[T]] = remains match {
      case Nil => result
      case x :: xs if(result.isEmpty || result.last.head != x)=> packer(result ::: List(List(x)), xs)
      case x :: xs => packer(result.init ::: List(result.last ::: List(x)), xs)
    }
    packer(List(), list)
  }


  //10
  def encode[T](list: List[T]): List[(T, Int)] = {
    def tupleCreator(xs: List[(T, Int)])(el: List[T]): List[(T, Int)] = {
      xs ::: List((el.head, el.length))
    }
    pack( list ).foldLeft(List[(T, Int)]())(tupleCreator(_)(_))
  }

  //11
  // def encodeModified[T](list: List[List[T]]): List[Any] = {
  //   def anyCreator(xs: List[Any])(el: List[T]): List[Any] = el.length match {
  //     case 1 => xs ::: List(el.head)
  //     case 2 => xs ::: List((el.head, el.length))
  //   }
  //   pack(list).foldLeft(List[Any]())(anyCreator(_)(_))
  // }

  //12
  def decode[T](list: List[(T, Int)]): List[T] =  {
    def tupleDecoder(res: List[T])(tuple: (T, Int)): List[T] = tuple match {
      case (el, len) if(len == 0) => res
      case (el, len)  => tupleDecoder(res ::: List(el))(( tuple._1, len - 1 ))
    }
    list.foldLeft(List[T]())(tupleDecoder(_)(_))
  }

  //13
  def encodeDirectly[T](list: List[T]): List[(T, Int)] = {
    def encoder(result: List[(T, Int)], remain: List[T]): List[(T, Int)] = remain match {
      case Nil => result
      case x :: xs if(result.isEmpty || result.last._1 != x) => encoder(result ::: List((x,1)), xs)
      case x :: xs => {
        val n = result.last._2 + 1
        encoder(result.init ::: List((result.last._1, n)), xs)
      }
    }
    encoder(List(), list)
  }

  //14
  def duplicate[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case x :: xs => x :: x :: duplicate(xs)
  }

  //15
  def duplicateN[T](list: List[T], n: Int): List[T] = {
    def duplicater(el: T, n: Int): List[T] = n match {
      case 0 => Nil
      case _ => el :: duplicater(el, n - 1)
    }

    list match {
      case Nil => Nil
      case x :: xs => duplicater(x,n) ::: duplicateN(xs, n)
    }
  }

  //16
  def dropN[T](list: List[T], n: Int): List[T] = ( list, n ) match {
    case (Nil, _) => Nil
    case (xs, i) => if(i == 0) dropN(xs.tail, n) else xs.head :: dropN(xs.tail, n - 1)
  }


  //17
  def split[T](list: List[T], n: Int): (List[T], List[T]) = {
    def drop(list: List[T], n: Int): List[T] = (list, n) match {
      case (_, 0) => list
      case (xs, n) => drop(list, n -1)
    }
    def take(list: List[T], n: Int): List[T] = (list, n) match {
      case (_, 0) => Nil
      case (xs, n) => xs.head :: take(xs.tail, n - 1)
    }

    (take(list,n), drop(list, n))
  }

  //18
  def slice[T](start: Int, end: Int, list: List[T]): List[T] = {
    def creator(start: Int, end: Int, list: List[T], res: List[T]): List[T] = (start, end, list, res) match {
      case (_, _, Nil, _) => Nil
      case (_, 0, _,res) => res
      case (s, e, ll, res) if(s > 0) => creator(s - 1, e - 1, ll.tail, res)
      case (s, e, ll, res) if(s == 0 && e > 0) => creator(s, e - 1, ll.tail, res ::: List(ll.head))
    }
    creator(start, end, list, Nil)
  }

  //19
  def rotate[T](n: Int, list: List[T]): List[T] = (n, list) match {
    case (_, Nil) => Nil
    case (0, xs) => xs
    case (n, xs) if(n > 0)=> rotate(n - 1, xs.tail ::: List(xs.head))
    case (n, xs) if(n < 0) => rotate(n + 1, xs.last :: xs.init)
  }

  //20
  def remove[T](n: Int, list: List[T]): (T, List[T]) = {
    def remover(n: Int, xs: List[T], header: List[T]): (T, List[T]) = (n, xs) match {
      case (_, Nil) => throw new Error("Nil list") 
      case (0, xs) => (xs.head, header ::: xs.tail)
      case (n, xs) => remover(n - 1, xs.tail, header ::: List(xs.head))
    }
    remover(n, list, Nil)
  }

  //21
  def insertAt[T](el: T, n: Int, list: List[T]): List[T] = {
    def inserter(n: Int, xs: List[T], header: List[T]): List[T] = (n, xs) match {
      case (_, Nil) => throw new Error("Not possible")
      case (0, xs) => header ::: List(el) ::: xs
      case (n, xs) => inserter(n - 1, xs.tail, header ::: List(xs.head))
    }
    inserter(n, list, Nil)
  }

  //22
  def range(start: Int, end: Int): List[Int] = (start, end) match {
    case (s,e) if(e - s < 0) => throw new Error("not possible")
    case (s,e) if(e - s == 0)=> e :: Nil
    case (s,e) if(e - s > 0)=> s :: range(s + 1, e)
  }

  //23
  def randomSelect[T](n: Int, list: List[T]): List[T] = {
    import scala.util.Random
    val rand = new Random

    def helper(n: Int, xs: List[T]): List[T] = (n, xs) match {
      case (_, Nil) => Nil
      case (0, xs) => Nil
      case (n, xs) =>
        val i = rand.nextInt(xs.length)
        val(el, rest) = remove(i, xs)
        el :: (helper(n - 1, rest))
    }
    helper(n, list)
  }

  //24
  def lotto(n: Int, end: Int): List[Int] = {
    val xs = range(1, end)
    randomSelect(n, xs)

  }

  //25
  def randomPermute[T](list: List[T]): List[T] = {
    randomSelect(list.length, list)
  }

  //26
  def combinations[T](n: Int, list: List[T]): List[List[T]] = {
    def creator(n: Int, xs: List[T], whatBefor: List[T]): List[List[T]] = {
      xs.length match {
        case i if(i == n)=> List(xs) 
        case _ =>
          val head = xs take (n - 1)
          val tail = xs drop (n - 1)
          val resHead = for(el <- whatBefor) yield head ::: List(el)
          val resTail = for(el <- tail) yield head ::: List(el)
          resHead ::: resTail ::: creator(n,xs.tail, whatBefor ::: List(xs.head))
      }
    }
    creator(n, list, List())
  }

}


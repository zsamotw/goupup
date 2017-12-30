package varia

/**
  *Chess horse algorithm. It starts from the point and try to visit all fields of board.
  */

object ChessHorse {
  val eval_1: List[Int => Int] = List((x: Int) => x + 1, (x: Int) => x - 1)
  val eval_2: List[Int => Int] = List((y: Int) => y + 2, (y: Int) => y - 2)

  case class Point(x: Int, y: Int)

  def moves(point: Point, n: Int): List[(Int, Int)] = {
    val all = {
      for {e1 <- eval_1; e2 <- eval_2} yield (e1(point.x), e2(point.y))
    }
    all.filter(t => t match {
                 case (x,y) => x > 0 && x <= n && y > 0 && y <=n
               })
  }

  def allFields(start: Point, n: Int): List[List[(Int, Int)]] = {
    var paths = List[List[(Int,Int)]]()
    def procedure(point: Point, k: Int): List[List[(Int, Int)]] = {
      if (k == 0)  List[List[(Int, Int)]](List((1,1)))
      else {
        for {
          move <- moves(point, n)
          path <- procedure(Point(move._1, move._2), k*k - 1)
          if (!(path contains (move._1, move._2)))
            } yield (move._1, move._2) :: path
      }
    }
    procedure(start, n)
  }

  allFields(Point(3,4),8)

}

/**
  *Alghoritm for looking for a way out from maze. The maze is two dimensional array.
  */


object Minotaur {
  val minotaur = Array.ofDim[Char](10,10)
  val rand = new scala.util.Random

  for(x <- 0 until 10; y <- 0 until 10) {
    minotaur(x)(y) = 'o'
  }

  for(i <- 1 to 10) {
    minotaur(rand.nextInt(9))(rand.nextInt(9)) = '|'
  }

  minotaur(1)(1) = 'F'

  def route(x: Int, y: Int, field: Array[Array[Char]], path: List[(Int, Int)]): Boolean = {

    def isFinish(x: Int, y: Int) = field(x)(y) == 'F'

    def isWall(x: Int, y: Int): Boolean =   y == 9 || y == 0 || x == 0 || x == 9 || field(x)(y) == '|'

    def wasIHere(x: Int, y: Int): Boolean = field(x)(y) == '!'

    println("route start for: " + x + " " + y)

    println("pole: " + field(x)(y))

    if(isWall(x,y)) {
      println("it is wall")
      false
    }

    else if(wasIHere(x,y)) {
      println("I was here....")
      false
    }

    else if(isFinish(x,y)) {
      println("Finito")
      true
    }

    else {
      field(x)(y) = '!'

      if(route(x + 1, y, field, (x,y)::path)) true
      else if(route(x, y + 1, field, (x,y)::path)) true
      else if(route(x - 1, y, field, (x,y)::path)) true
      else if(route(x, y - 1, field, (x,y)::path)) true
      else {
        field(x)(y) = 'o'
        false
      }
    }
  }

  for(x <- 0 to 9; y <- 0 to 9) {
    if (y == 9) print(minotaur(x)(y) + "\n")
    else print(minotaur(x)(y))
  }

  val road = route(5,5,minotaur, List())

  for(x <- 0 until 10; y <- 0 until 10) {
    if (y == 9) print(minotaur(x)(y) + "\n")
    else print(minotaur(x)(y))
  }
}


/**
  * implementations of exercises of ninety nine scala problems from http://aperiodic.net/phil/scala/s-99/
  */

object Logic {

  //46
  def and(p: Boolean, pp: Boolean):Boolean = p && pp
  def not(p: Boolean) = !p
  def or(p: Boolean, pp: Boolean) = p || pp
  def nand(p: Boolean, pp: Boolean) = not(and(p,pp))
  def nor(p: Boolean, pp: Boolean) = not(or(p,pp))
  def xor(p: Boolean, pp: Boolean) = or(p,pp) && not(equ(p,pp))
  def equ(p: Boolean, pp: Boolean) = p == pp

  //49
  def greyCode(n: Int) = {
    def loop(acc: List[List[Int]], n: Int): List[List[Int]] = {
      n match {
        case 0 => acc
        case n =>
          val updatedLists = for{
            a <- acc
            i <- List(0,1)
          } yield a.updated(n - 1, i)
          loop(updatedLists, n - 1)
      }
    }
    val initial = List.fill(n)(0)
    loop(List(initial), n)
  }
}

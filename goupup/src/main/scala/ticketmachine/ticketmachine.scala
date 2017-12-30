package ticketmachine

import scala.util.{Success, Failure, Try}

class Machine(val coins: List[(Int, List[Int])], val tickets: List[(Int, List[Int])], val menu: Menu){
  self =>
  def printMenu = menu.print

  def getChoice: Int = {
    val choiceInTry =
    Try{
      Console.readInt()
    }
    choiceInTry match {
      case Success(ch) => ch
      case Failure(f) => 0
    }
  }

  override def toString: String = {
    s"""coins: ${coins.toString}
    tickets: ${tickets.toString}"""
  }

  def buy(ticket: Int, money: Int): Machine = {
    if(machineContainsTicket(ticket)) {
      val moneyToGive = money - ticket
      if(moneyToGive < 0) this
      else {
        val newCoins = giveCharge(coins, moneyToGive)
        val newTickets = giveTicket(tickets, ticket)
        new Machine(newCoins, newTickets, menu)
      }
    }
    else this
  }

  def machineContainsTicket(ticket: Int): Boolean = {
    tickets.foldRight(false)((elem, acc) => acc || {
      if(elem._1 == ticket) {
        if(elem._2.isEmpty) false else true
      }
      else false
        })
  }

  def giveCharge(coins: List[(Int, List[Int])], moneyToGive: Int): List[(Int, List[Int])] = {
    val sortedCoins = coins.sortWith{case (prev, next) => prev._1 > next._1}
    (sortedCoins, moneyToGive) match {
      case (Nil, _) => self.coins
      case (_, 0) => coins
      case ((elem :: elems), moneyToGive) =>
        elem match {
          case (value, Nil) => elem :: giveCharge(elems, moneyToGive)
          case (value, list) =>
            if(value > moneyToGive) elem :: giveCharge(elems, moneyToGive)
            else {
              val newCoins = giveCoin(value, coins)
              giveCharge(newCoins, moneyToGive - value)
            }
        }
    }
  }

  def giveTicket(tickets: List[(Int, List[Int])], ticket: Int): List[(Int, List[Int])]= {
    tickets map{case (value, list) => if(value == ticket) (value, list drop(1)) else (value, list)}
  }

  def giveCoin(value: Int, coins: List[(Int, List[Int])]): List[(Int, List[Int])] = {
    coins map{case (v, list) => if(value == v) (v, list drop(1)) else (v,list)}
  }
}

class Menu(val items: List[String]) {
  def print: Unit = for(item <- items) println(item)
}

class TicketApp {
    def loop(machine: Machine): Unit = {
    println(s"Machine state: ${machine.toString}")
    machine.printMenu
    val choice = machine.getChoice
    choice match { 
      case 0 =>
        println("error with input value")
        loop(machine)
      case 1 =>
        println("Ticket value?")
        val ticket = machine.getChoice
        println("How much money do you have?")
        val money = machine.getChoice
        val newMashine = machine.buy(ticket, money)
        loop(newMashine)
      case 2 => println("exit")
      case _ =>
        println("there is no such option")
        loop(machine)
    }
  }

}

object AppMachine extends App {
  val coins = List((10, List(10, 10, 10)), (5, List(5,5,5,5)), (1, List(1,1,1,1,1,1,1,1,1,1)))
  val tickets = List((10, List(10)), (8, List(8,8)))
  val menu = new Menu(List("1) buy", "2) exit"))
  val machine = new Machine(coins, tickets, menu)
  val app = new TicketApp
  app.loop(machine)
}

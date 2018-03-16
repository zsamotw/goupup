package varia

class Society(var persons: Set[Person]) {
  def add(person: Person) = persons += person
  
  def printAll = persons.foreach(println(_))
}

case class Person(name: String, age:Int, var friends: Set[Person]) {
}

case class Notes() {
  def write(person: Person) = println()
}


package varia

import org.scalatest._


class SocietyTest extends FlatSpec with Matchers{
  it should "person has proper name" in {
    val perter = Person("Peter", 14, Set.empty[Person])
    perter.name shouldBe "Peter"
  }
}
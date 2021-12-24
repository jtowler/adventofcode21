package aoc.aoc2021

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.io.Source
import scala.util.Using

class PlayerTest extends AnyFlatSpec with should.Matchers {

  "A Player" should "correctly update positions and scores" in {
    Player(0, 4).roll(6) shouldBe Player(10, 10)
    Player(0, 8).roll(15) shouldBe Player(3, 3)
    Player(10, 10).roll(24) shouldBe Player(14, 4)
  }

  "getNext3" should "return a score and the next dice value" in {
    val (score1, dice1) = Day21.getNext3(1)
    score1 shouldBe 1 + 2 + 3
    dice1 shouldBe 4
  }

}
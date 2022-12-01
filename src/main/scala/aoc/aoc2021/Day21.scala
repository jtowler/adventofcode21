package aoc.aoc2021

import aoc.Day
import aoc.aoc2021.Day21.getNext3
import com.sun.org.apache.xml.internal.security.algorithms.Algorithm

import scala.annotation.tailrec


case class Player(score: Int, position: Int):
  def roll(dice: Int): Player =
    val mod = (dice + position) % 10
    val newPos = if mod == 0 then 10 else mod
    Player(score + newPos, newPos)

class Day21 extends Day(21, 2021) {

  @tailrec
  final def game(dice: Int, rollValue: Int, rolls: Int, player1: Player, player2: Player): Int =
    val player1roll = player1.roll(rollValue)
    println(s"Player 1 rolls $rollValue and moves to space ${player1roll.position} for a total score of ${player1roll.score}")
    if player1roll.score >= 1000 then
      return player2.score * (rolls + 3)
    val (rollValue1, nextDice) = getNext3(dice)
    val player2roll = player2.roll(rollValue1)
    println(s"Player 2 rolls $rollValue1 and moves to space ${player2roll.position} for a total score of ${player2roll.score}")
    if player2roll.score >= 1000 then
      player1roll.score * (rolls + 6)
    else
      val (rollValue2, nexterDice) = getNext3(nextDice)
      game(nexterDice, rollValue2, rolls + 6, player1roll, player2roll)

  override def answer1(): Any = game(4, 6, 0, Player(0, 7), Player(0, 1))

  override def answer2(): Any = ""
}
object Day21 {
  def getNext3(n: Int): (Int, Int) = n match {
    case 98 => (297, 1)
    case 99 => (200, 2)
    case 100 => (103, 3)
    case _ => ((n to n + 2).sum, n + 3)
  }

}
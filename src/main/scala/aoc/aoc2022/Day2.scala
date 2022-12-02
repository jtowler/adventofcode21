package aoc.aoc2022

import aoc.Day

import scala.annotation.tailrec

sealed trait Move(val v: Int) {

  def move(that: Move): Int = (this, that) match
    case (l: Move, r: Move) if l == r => 3 + v
    case (_: Rock, _: Paper) => 0 + v
    case (_: Rock, _: Scissors) => 6 + v
    case (_: Paper, _: Rock) => 6 + v
    case (_: Paper, _: Scissors) => 0 + v
    case (_: Scissors, _: Rock) => 0 + v
    case (_: Scissors, _: Paper) => 6 + v

  def choose(outcome: Char): Move = (this, outcome) match
    case (m: Move, 'Y') => m
    case (_: Rock, 'X') => Scissors()
    case (_: Rock, 'Z') => Paper()
    case (_: Paper, 'X') => Rock()
    case (_: Paper, 'Z') => Scissors()
    case (_: Scissors, 'X') => Paper()
    case (_: Scissors, 'Z') => Rock()
}

object Move {
  def fromC(c: Char): Move = c match
    case 'A' => Rock()
    case 'X' => Rock()
    case 'B' => Paper()
    case 'Y' => Paper()
    case 'C' => Scissors()
    case 'Z' => Scissors()
}

case class Rock() extends Move(1)
case class Paper() extends Move(2)
case class Scissors() extends Move(3)

class Day2 extends Day(2, 2022) {

  private val raw = readData().getLines().toList
  val youChars = raw.map(_.last)
  val thems = raw.map(x => Move.fromC(x.head))
  val yous = youChars.map(Move.fromC)

  val scores = thems.zip(yous).map((t, y) => y.move(t))
  val yous2 = thems.zip(youChars).map((t, c) => t.choose(c))
  val scores2 = thems.zip(yous2).map((t, y) => y.move(t))

  //  11666
  override def answer1(): Any = scores.sum

  //  12767
  override def answer2(): Any = scores2.sum

}

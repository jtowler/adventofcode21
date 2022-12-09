package aoc.aoc2022

import aoc.Day

import scala.annotation.tailrec

type C = (Int, Int)

val dirs = Map('U' -> (0, 1), 'D' -> (0, -1), 'L' -> (-1, 0), 'R' -> (1, 0))

case class Rope(h: C, t: C) {

  def attached: Boolean = (h._1 - t._1).abs <= 1 && (h._2 - t._2).abs <= 1

  def move(d: Char): Rope = {
    val (xD, yD) = dirs(d)
    val newRope = this.copy(h = (h._1 + xD, h._2 + yD))
    if (h == t || newRope.attached) {
      newRope
    } else {
      newRope.copy(t = (newRope.h._1 - xD, newRope.h._2 - yD))
    }
  }
}

class Day9 extends Day(9, 2022) {

  @tailrec
  private def move(rope: Rope, d: Char, n: Int, tails: List[C]): (Rope, List[C]) = n match
    case i if i <= 0 => (rope, tails)
    case i =>
      val newRope = rope.move(d)
      move(newRope, d, i - 1, newRope.t :: tails)

  @tailrec
  private def getTails(rope: Rope, ins: List[(Char, Int)], tails: List[C] = List.empty): List[C] = ins match
    case Nil => tails
    case h :: t =>
      val (newRope, newTails) = move(rope, h._1, h._2, tails)
      getTails(newRope, t, newTails)

  private val ins: List[(Char, Int)] = readData().getLines().map { s =>
    val spl = s split " "
    (spl.head.head, spl(1).toInt)
  }.toList


  //  5619
  override def answer1(): Any = getTails(Rope((0, 0), (0, 0)), ins).distinct.size

  //  327180
  override def answer2(): Any = None
}

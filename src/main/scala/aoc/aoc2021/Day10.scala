package aoc.aoc2021

import aoc.Day

import scala.annotation.tailrec

val openers = List('(', '[', '{', '<')
val closers = List(')', ']', '}', '>')
val scoreMap = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)
val openToClose: Map[Char, Char] = (openers zip closers).toMap

case class MyStack(expecting: List[Char] = List()) {

  def wasExpecting(ch: Char): Boolean = ch match {
    case c if openers contains c => true
    case c if (closers contains c) && (expecting.head == c) => true
    case _ => false
  }

  def update(ch: Char): MyStack =
    val newList = if openers contains ch then openToClose(ch) :: expecting else expecting.tail
    MyStack(newList)

  lazy val score: BigInt = expecting.foldLeft(BigInt(0))((total, h) => (total * 5) + scoreMap(h))

}

class Day10 extends Day(10, 2021) {

  private val data = readData().getLines().map(_.toCharArray.toList).toList
  private val closerMap = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)

  @tailrec
  private def getCorrupted(chars: List[Char], stack: MyStack): Option[Int] = chars match {
    case Nil => None
    case h :: t if !stack.wasExpecting(h) => Some(closerMap(h))
    case h :: t => getCorrupted(t, stack.update(h))
}

  override def answer1(): Any = data.flatMap(line => getCorrupted(line, MyStack())).sum

  override def answer2(): Any =
    val scores = data.filter(line => getCorrupted(line, MyStack()).isEmpty).map {
      _.foldLeft(MyStack())((st, h) => st.update(h)).score
    }.sorted
    scores(scores.length / 2)
}
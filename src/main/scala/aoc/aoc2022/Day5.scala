package aoc.aoc2022

import aoc.Day

case class MoveCrate(n: Int, from: Int, to: Int) {
  def moveMultiple(stacks: Stacks): Stacks = stacks.moveMultiple(n, from, to)
  def moveMultipleReverse(stacks: Stacks): Stacks = stacks.moveMultiple(n, from, to, true)
}

case class Stacks(stacks: List[List[Char]] = List.empty) {

  def moveMultiple(n: Int, from: Int, to: Int, reverse: Boolean = false): Stacks = {
    val (fromH, fromT) = stacks(from).splitAt(n)
    val toL = if reverse then fromH.reverse ::: stacks(to) else fromH ::: stacks(to)
    Stacks(stacks.indices.map { i =>
      i match
        case _ if i == from => fromT
        case _ if i == to => toL
        case _ => stacks(i)
    }.toList)
  }

  def tops(): String = stacks.map(_.head).mkString
}

class Day5 extends Day(5, 2022) {

  private val raw = readData().getLines().toList

  private val initStacks = raw.takeWhile(_.contains('['))
  private val nCrates = raw(initStacks.size).filter(_.isDigit).map(_.toString.toInt).max

  private val pattern = """move (\d+) from (\d+) to (\d+)""".r
  private val moves = raw.slice(initStacks.size + 2, raw.length).map { s =>
    val pattern(n, f, t) = s
    MoveCrate(n.toInt, f.toInt - 1, t.toInt - 1)
  }

  private val crateInds = 1 to nCrates * 4 by 4

  val stacks = Stacks(initStacks.map(l => crateInds.map(l(_))).transpose.map(_.dropWhile(_ == ' ')))

  val stacks2 = moves.foldLeft(stacks)((s, m) => m.moveMultipleReverse(s))
  val stacks3 = moves.foldLeft(stacks)((s, m) => m.moveMultiple(s))

  //  BZLVHBWQF
  override def answer1(): Any = stacks2.tops()

  //  TDGJQTZSL
  override def answer2(): Any = stacks3.tops()

}

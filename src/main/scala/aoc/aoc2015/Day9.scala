package aoc.aoc2015

import aoc.Day

import scala.annotation.tailrec

case class Edge(from: String, to: String, dist: Int)

class Day9 extends Day(9, 2015) {
  private val edgePattern = """(\w+) to (\w+) = (\d+)""".r
  private val edges = readData().getLines().flatMap { x =>
    val edgePattern(from, to, dist) = x
    List(Edge(from, to, dist.toInt), Edge(to, from, dist.toInt))
  }.toList
  private val allLocs = edges.map(_.from).distinct.permutations.map{ perm =>
    (perm.init zip perm.tail).map((f, t) => edges.find(e => (e.from == f) && (e.to == t)).get.dist).sum
  }.toList

  override def answer1(): Any = allLocs.min

  override def answer2(): Any = allLocs.max

}
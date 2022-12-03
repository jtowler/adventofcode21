package aoc.aoc2022

import aoc.Day

import scala.annotation.tailrec


class Day3 extends Day(3, 2022) {

  def getScore(c: Char): Int = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".indexOf(c)

  private val raw = readData().getLines().toList

  private val scores = raw.map {
    x => x.splitAt(x.length / 2)
  }.map {
    (s1, s2) => s1.filter(s2 contains _).toSet.map(getScore).toList
  }

  private val scores2 = raw.grouped(3).map {
    ls => ls.head.filter(ls(1) contains _).filter(ls(2) contains _).toSet.map(getScore).toList
  }

  //  7908
  override def answer1(): Any = scores.flatten.sum

  //  2838
  override def answer2(): Any = scores2.flatten.sum

}

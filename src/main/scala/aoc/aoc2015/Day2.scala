package aoc.aoc2015

import aoc.Day

import scala.annotation.tailrec

case class Box(l: Int, w: Int, h: Int) {
  val face1: Int = l * w
  val face2: Int = w * h
  val face3: Int = h * l
  val faces = List(face1, face2, face3)
  val perims = List(l + w, w + h, h + l)
  val area: Int = 2 * faces.sum + faces.min

  val perim = l * w * h + perims.min * 2
}

class Day2 extends Day(2, 2015) {

  val pattern = """(\d+)x(\d+)x(\d+)""".r
  val data = readData().getLines().map{ s =>
    val pattern(l, w, h) = s
    Box(l.toInt, w.toInt, h.toInt)
  }.toList

  override def answer1(): Any = data.map(_.area).sum

  override def answer2(): Any = data.map(_.perim).sum
}
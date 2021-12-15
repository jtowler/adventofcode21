package aoc.aoc2021

import aoc.Day

import scala.annotation.tailrec

case class Q(vs: Map[(Int, Int), Int]) {

  def getMin: ((Int, Int), Int) = vs.minBy(_._2)

  def update(u: ((Int, Int), Int), data: List[List[Int]]): Q =
    val (uY, uX) = u._1
    val newMap = vs.map {
      v =>
        if (v._1 == (uY + 1, uX)) | (v._1 == (uY, uX + 1)) |
          (v._1 == (uY, uX - 1)) | (v._1 == (uY - 1, uX)) then
          val alt = u._2 + data(v._1._1)(v._1._2)
          if alt < v._2 then v._1 -> alt else v
        else v
    }
    Q(newMap - u._1)
}


class Day15 extends Day(15, 2021) {

  private val data = readData().getLines().map(_.toCharArray.map(_.asDigit).toList).toList
  private val xSize = data.head.size
  private val ySize = data.size

  def generateQs(ySize: Int, xSize: Int): Q =
    val qs = (for {
      i <- 0 until ySize
      j <- 0 until xSize
      v = if (i == 0) && (j == 0) then 0 else Int.MaxValue
    } yield (i, j) -> v).toMap
    Q(qs)

  @tailrec
  private def loop(thisQ: Q, thisData: List[List[Int]], yLimit: Int, xLimit: Int): Int = {
    val minV = thisQ.getMin
    if minV._1 == (yLimit, xLimit) then
      thisQ.getMin._2
    else
      loop(thisQ.update(minV, thisData), thisData, yLimit, xLimit)
  }

  override def answer1(): Any =
    val q = generateQs(ySize, xSize)
    loop(q, data, ySize - 1, xSize - 1)

  override def answer2(): Any =
    val newData = data.map { d =>
      (0 to 4).flatMap(n => d.map(_ + n)).map(x => if x > 9 then x - 9 else x)
    }
    val newerData = newData.transpose.map { d =>
      (0 to 4).flatMap(n => d.map(_ + n)).map(x => if x > 9 then x - 9 else x)
    }.transpose

    val ySize = newerData.size
    val xSize = newerData.head.size

    val q = generateQs(ySize, xSize)
    loop(q, newerData, ySize - 1, xSize - 1)
}
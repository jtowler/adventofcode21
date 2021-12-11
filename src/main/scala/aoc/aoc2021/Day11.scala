package aoc.aoc2021

import aoc.Day

import scala.annotation.tailrec

class Day11 extends Day(11, 2021) {

  private val data = readData().getLines().map(_.toCharArray.map(_.asDigit).toList).toList
  private val xSize = data.size
  private val ySize = data.head.size
  private val gridSize = xSize * ySize

  @tailrec
  private def iterOcts(octs: List[List[Int]], flashers: List[(Int, Int)]): (Int, List[List[Int]]) =
    val newFlashers = for {
      x <- octs.indices
      y <- octs.head.indices
      if octs(x)(y) > 9 & !(flashers contains (x, y))
    } yield (x, y)

    if newFlashers.isEmpty then
      val finalOcts = octs.map (_.map(cell => if cell > 9 then 0 else cell))
      (flashers.size, finalOcts)
    else
      val newOct = newFlashers.foldLeft(octs)((oct, coord) => applyFlash(oct, coord(0), coord(1)))
      iterOcts(newOct, flashers ++ newFlashers)

  def isInBounds(x: Int, y: Int): Boolean = x >= 0 & y >= 0 & x < xSize & y < ySize

  def getAdj(x: Int, y: Int): List[(Int, Int)] = (for {
    i <- -1 to 1
    j <- -1 to 1
    if !((i == 0) & (j == 0))
  } yield (x + i, y + j)).filter(isInBounds).toList

  def applyFlash(octs: List[List[Int]], x: Int, y: Int): List[List[Int]] =
    val adjs = getAdj(x, y)
    octs.zipWithIndex.map { (line, oX) =>
      line.zipWithIndex.map { (cell, oY) =>
        if adjs contains(oX, oY) then cell + 1
        else cell
      }
    }


  override def answer1(): Any =

    @tailrec
    def getFlashes(it: Int, its: Int, flashes: Int, octs: List[List[Int]]): Int = it match {
      case i if i == its => flashes
      case _ =>
        val (newFlash, newOcts) = iterOcts(octs.map (_.map(_ + 1)), List())
        getFlashes(it + 1, its, flashes + newFlash, newOcts)
    }
    getFlashes(0, 100, 0, data)


  override def answer2(): Any =
    @tailrec
    def getSync(it: Int, flashes: Int, octs: List[List[Int]]): Int = flashes match {
      case fl if fl == gridSize => it
      case _ =>
        val (newFlash, newOcts) = iterOcts(octs.map (_.map(_ + 1)), List())
        getSync(it + 1, newFlash, newOcts)
    }
    getSync(0, 0, data)

}
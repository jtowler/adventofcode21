package aoc.aoc2021

import aoc.Day
import com.sun.org.apache.xml.internal.security.algorithms.Algorithm

import scala.annotation.tailrec


case class Grid(grid: List[List[Int]], infFunc: Int => Int, inf: Int = 0) {

  val newInf: Int = infFunc(inf)

  val ySize: Int = grid.size
  val xSize: Int = grid.head.size

  def sum: Int = grid.map(_.sum).sum

  def getAtCoord(y: Int, x: Int): Int = if (y >= 0) && (x >= 0) && (y < ySize) && (x < xSize) then grid(y)(x) else inf

  def getPixelAtCoord(y: Int, x: Int, algorithm: List[Int]): Int =
    val coordString = (for {
      i <- -1 to 1
      j <- -1 to 1
    } yield getAtCoord(y + i, x + j)).mkString
    val coordInt = Integer.parseInt(coordString, 2)
    algorithm(coordInt)

  def update(algorithm: List[Int]): Grid =
    val newGrid = for {
      i <- -1 to ySize
      j <- -1 to xSize
    } yield getPixelAtCoord(i, j, algorithm)
    Grid(newGrid.toList.grouped(ySize + 2).toList, infFunc, newInf)

  @tailrec
  final def multiUpdate(algorithm: List[Int], target: Int, n: Int = 0): Grid = n match {
    case i if i == target => this
    case _ => update(algorithm).multiUpdate(algorithm, target, n + 1)
  }

  def display(): Unit = grid.foreach(l => println(l.mkString))
}


class Day20 extends Day(20, 2021) {

  val data: List[String] = readData().getLines().toList

  val algo: List[Int] = data.head.map(v => if v == '#' then 1 else 0).toList
  val inputImage: List[List[Int]] = data.slice(2, data.size).map(_.map(v => if v == '#' then 1 else 0).toList)

  override def answer1(): Any = Grid(inputImage, x => if x == 1 then 0 else 1).multiUpdate(algo, 2).sum

  override def answer2(): Any = Grid(inputImage, x => if x == 1 then 0 else 1).multiUpdate(algo, 50).sum
}
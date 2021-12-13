package aoc.aoc2021

import aoc.Day

def getBoard(coords: List[(Int, Int)]): List[List[Boolean]] =
  List.tabulate(coords.map(_._2).max + 1, coords.map(_.head).max + 1)((y, x) =>
    if coords contains (x, y) then true else false)

def dispCoord(coords: List[List[Boolean]]): Unit =
  coords.foreach ( x => println(x.map(if _ then '#' else ' ').mkString))

class Day13 extends Day(13, 2021) {

  private val data = readData().getLines().toList
  private val splitInd = data.indexWhere(_ contains ' ')
  private val coords = data.slice(0, splitInd - 1).map{ coord =>
    val ins = coord.split(" ").last.split(",")
    (ins.head.toInt, ins.last.toInt)
  }
  private val folds = data.slice(splitInd, data.length).map { line =>
    val ins = line.split(" ").last.split("=")
    (ins.head.head, ins.last.toInt)
  }

  def fold(cds: List[List[Boolean]], axis: Char, line: Int): List[List[Boolean]] =
    if axis == 'y' then
      val normal = cds.slice(0, line)
      val folded = cds.slice(line + 1, cds.size).reverse
      (normal zip folded).map((xs, ys) => (xs zip ys).map(_ | _))
    else
      val normal = cds.map(_.slice(0, line))
      val folded = cds.map(l => l.slice(line + 1, l.size).reverse)
      (normal zip folded).map((xs, ys) => (xs zip ys).map(_ | _))

  override def answer1(): Any =
    fold(getBoard(coords), folds.head._1, folds.head._2).map(_.count(_ == true)).sum

  override def answer2(): Any =
    dispCoord(folds.foldLeft(getBoard(coords))((b, f) => fold(b, f._1, f._2)))
}
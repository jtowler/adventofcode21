package aoc.aoc2022

import aoc.Day


def anyVisible(x: Int, y: Int, grid: List[List[Int]], tGrid: List[List[Int]]): Boolean = {
  def isVisibleLeft(x: Int, y: Int, grid: List[List[Int]]): Boolean = {
    val g = grid(x)
    !g.slice(0, y).exists(_ >= g(y))
  }

  def isVisibleRight(x: Int, y: Int, grid: List[List[Int]]): Boolean = {
    val g = grid(x)
    !g.slice(y + 1, g.size).exists(_ >= g(y))
  }

  isVisibleLeft(x, y, grid) || isVisibleRight(x, y, grid) ||
    isVisibleLeft(y, x, tGrid) || isVisibleRight(y, x, tGrid)
}


def scenicScore(x: Int, y: Int, grid: List[List[Int]], tGrid: List[List[Int]]): Int = {
  def leftDist(x: Int, y: Int, grid: List[List[Int]]): Int = {
    val g = grid(x)
    takeUntil(g.slice(0, y).reverse, g(y)).size
  }

  def rightDist(x: Int, y: Int, grid: List[List[Int]]): Int = {
    val g = grid(x)
    takeUntil(grid(x).slice(y + 1, grid(x).size), g(y)).size
  }

  def takeUntil(arr: Seq[Int], v: Int): Seq[Int] = {
    val (a1, a2) = arr.span(_ < v)
    if a2.isEmpty then a1
    else a1 :+ a2.head
  }

  leftDist(x, y, grid) * rightDist(x, y, grid) * leftDist(y, x, tGrid) * rightDist(y, x, tGrid)
}


class Day8 extends Day(8, 2022) {

  private val raw = readData().getLines().map(_.map(_.toString.toInt).toList).toList

  val a1: List[List[Boolean]] = List.tabulate(raw.length, raw.head.length) {
    (x, y) => anyVisible(x, y, raw, raw.transpose)
  }

  val a2: List[List[Int]] = List.tabulate(raw.length, raw.head.length) {
    (x, y) => scenicScore(x, y, raw, raw.transpose)
  }

  //  1672
  override def answer1(): Any = a1.flatten.count(_ == true)

  //  327180
  override def answer2(): Any = a2.flatten.max
}

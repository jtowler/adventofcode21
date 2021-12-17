package aoc.aoc2021

import aoc.Day

import scala.annotation.tailrec

//val (tX1: Int, tX2: Int, tY1: Int, tY2: Int) = (20, 30, -10, -5)
val (tX1: Int, tX2: Int, tY1: Int, tY2: Int) = (79, 137, -176, -117)

case class Probe(x: Int, y: Int, xVel: Int, yVel: Int) {
  def update(): Probe =
    val newXVel = xVel match {
      case v if v < 0 => v + 1
      case v if v > 0 => v - 1
      case _ => xVel
    }
    Probe(x + xVel, y + yVel, newXVel, yVel - 1)
  def inTarget: Boolean = (x >= tX1) && (x <= tX2) && (y >= tY1) && (y <= tY2)
  def pastTarget: Boolean = y < tY1
}

case class Trajectory(probes: List[Probe]) {
  def update(): Trajectory = Trajectory(probes.head.update() :: probes)
  def inTarget: Boolean = probes.head.inTarget
  def pastTarget: Boolean = probes.head.pastTarget
  def maxY: Int = probes.map(_.y).max
}
object Trajectory {
  def fromXY(x: Int, y: Int): Trajectory = loop(Trajectory(List(Probe(0, 0, x, y))))

  @tailrec
  private def loop(t: Trajectory): Trajectory = t match {
    case trj if trj.pastTarget || trj.inTarget => trj
    case _ => loop(t.update())
  }
}

class Day17 extends Day(17, 2021) {

  val bruteForce: Seq[Trajectory] = for {
    i <- -200 to 200
    j <- -200 to 200
    t = Trajectory.fromXY(i, j)
    if t.inTarget
  } yield t

  override def answer1(): Any = bruteForce.maxBy(_.maxY).maxY

  override def answer2(): Any = bruteForce.size
}
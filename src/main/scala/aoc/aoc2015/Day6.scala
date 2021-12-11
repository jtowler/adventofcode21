package aoc.aoc2015

import aoc.Day

import scala.annotation.tailrec

case class Light2(ins: String, x1: Int, y1: Int, x2: Int, y2: Int) {
  def operateOne(in: Int): Int = ins match {
    case "turn on" => in + 1
    case "turn off" => if in == 0 then 0 else in - 1
    case "toggle" => in + 2
  }

  def operate(lights: List[List[Int]]): List[List[Int]] =
    List.tabulate(lights.size, lights.head.size) {
      (x, y) =>
        val inRange = x >= x1 && x <= x2 && y >= y1 && y <= y2
        val current = lights(x)(y)
        if inRange then operateOne(current) else current
    }
}
case class Light(ins: String, x1: Int, y1: Int, x2: Int, y2: Int) {

  def operateOne(in: Boolean): Boolean = ins match {
    case "turn on" => true
    case "turn off" => false
    case "toggle" => !in
  }

  def operate(lights: List[List[Boolean]]): List[List[Boolean]] =
    List.tabulate(lights.size, lights.head.size) {
      (x, y) =>
        val inRange = x >= x1 && x <= x2 && y >= y1 && y <= y2
        val current = lights(x)(y)
        if inRange then operateOne(current) else current
    }

}

class Day6 extends Day(6, 2015) {

  val pattern = """(.+) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)""".r
  val lightData = readData().getLines().map { s =>
    val pattern(ins, x1, y1, x2, y2) = s
    Light(ins, x1.toInt, y1.toInt, x2.toInt, y2.toInt)
  }.toList
  val light2Data = readData().getLines().map { s =>
    val pattern(ins, x1, y1, x2, y2) = s
    Light2(ins, x1.toInt, y1.toInt, x2.toInt, y2.toInt)
  }.toList

  val intLights = List.tabulate(1000, 1000)((_, _) => 0)
  val boolLights = List.tabulate(1000, 1000)((_, _) => false)

  override def answer1(): Any =
    @tailrec
    def loop(ins: List[Light], ls: List[List[Boolean]]): Int = ins match {
      case Nil => ls.flatten.count(_ == true)
      case h :: t => loop(t, h.operate(ls))
    }

    loop(lightData, boolLights)
    
  override def answer2(): Any =
    @tailrec
    def loop(ins: List[Light2], ls: List[List[Int]]): Int = ins match {
      case Nil => ls.flatten.sum
      case h :: t => loop(t, h.operate(ls))
    }
    loop(light2Data, intLights)
}
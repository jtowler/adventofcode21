package aoc.aoc2015

import aoc.Day

import scala.annotation.tailrec

class Day7 extends Day(7, 2015) {

  val initPattern = """(\d+) -> ([a-z]+)""".r
  val andPattern = """([a-z]+) AND ([a-z]+) -> ([a-z]+)""".r
  val orPattern = """([a-z]+) OR ([a-z]+) -> ([a-z]+)""".r
  val lshiftPattern = """([a-z]+) LSHIFT (\d+) -> ([a-z]+)""".r
  val rshiftPattern = """([a-z]+) RSHIFT (\d+) -> ([a-z]+)""".r
  val notPattern = """NOT ([a-z]+) -> ([a-z]+)""".r
  val data: List[String] = readData().getLines().toList
//  val wires: List[Wire] = readData().getLines().map {
//    case initPattern(name, value) => Init(name, value.toInt)
//    case andPattern(name, value) => And(name, value.toInt)
//  }.toList

  def updateWires(line: String, currWires: Map[String, Int]): Map[String, Int] = line match {
    case initPattern(value, name) => currWires.updated(name, value.toInt)
    case andPattern(input1, input2, output) => currWires.updated(output, currWires(input1) & currWires(input2))
    case orPattern(input1, input2, output) => currWires.updated(output, currWires(input1) | currWires(input2))
    case lshiftPattern(input, shift, output) => currWires.updated(output, currWires(input) << shift.toInt)
    case rshiftPattern(input, shift, output) => currWires.updated(output, currWires(input) >> shift.toInt)
    case notPattern(input, output) => currWires.updated(output, 65536 + ~currWires(input))
  }


  def loopWires(data: List[String], currWires: Map[String, Int]): Map[String, Int] = data match {
    case Nil => currWires
    case h :: t => loopWires(t, updateWires(h, currWires))
  }

  override def answer1(): Any = loopWires(data, Map())("a")
    
  override def answer2(): Any = ""
}
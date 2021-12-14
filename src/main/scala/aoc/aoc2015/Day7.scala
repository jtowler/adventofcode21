package aoc.aoc2015

import aoc.Day

import scala.annotation.tailrec

class Day7 extends Day(7, 2015) {
  val initPattern = """(\d+) -> ([a-z]+)""".r
  val init2Pattern = """([a-z]+) -> ([a-z]+)""".r
  val andPattern = """([a-z]+) AND ([a-z]+) -> ([a-z]+)""".r
  val and1Pattern = """(\d+) AND ([a-z]+) -> ([a-z]+)""".r
  val and2Pattern = """([a-z]+) AND (\d+) -> ([a-z]+)""".r
  val orPattern = """([a-z]+) OR ([a-z]+) -> ([a-z]+)""".r
  val or1Pattern = """(\d+) OR ([a-z]+) -> ([a-z]+)""".r
  val or2Pattern = """([a-z]+) OR (\d+) -> ([a-z]+)""".r
  val lshiftPattern = """([a-z]+) LSHIFT (\d+) -> ([a-z]+)""".r
  val rshiftPattern = """([a-z]+) RSHIFT (\d+) -> ([a-z]+)""".r
  val notPattern = """NOT ([a-z]+) -> ([a-z]+)""".r
  val data: List[String] = readData().getLines().toList

  @tailrec
  private def loopWires(data: List[String], wires: Map[String, Int]): Map[String, Int] = data match {
    case Nil => wires
    case d =>
      val (newWires, remaining) = getInitWires(d, wires, List())
      loopWires(remaining, newWires)
  }

  def getInitWires(data: List[String], wires: Map[String, Int],
                   remaining: List[String]): (Map[String, Int], List[String]) = data match {
    case Nil => (wires, remaining)
    case initPattern(value, name) :: t => getInitWires(t, wires.updated(name, value.toInt), remaining)
    case init2Pattern(input, output) :: t if (wires contains input) =>
      getInitWires(t, wires.updated(output, wires(input)), remaining)
    case andPattern(input1, input2, output) :: t if (wires contains input1) && (wires contains input2)  =>
      getInitWires(t, wires.updated(output, wires(input1) & wires(input2)), remaining)
    case and1Pattern(input1, input2, output) :: t if wires contains input2 =>
      getInitWires(t, wires.updated(output, input1.toInt & wires(input2)), remaining)
    case and2Pattern(input1, input2, output) :: t if wires contains input1 =>
      getInitWires(t, wires.updated(output, wires(input1) & input2.toInt), remaining)
    case orPattern(input1, input2, output) :: t if (wires contains input1) && (wires contains input2)  =>
      getInitWires(t, wires.updated(output, wires(input1) | wires(input2)), remaining)
    case or1Pattern(input1, input2, output) :: t if wires contains input2 =>
      getInitWires(t, wires.updated(output, input1.toInt | wires(input2)), remaining)
    case or2Pattern(input1, input2, output) :: t if wires contains input1 =>
      getInitWires(t, wires.updated(output, wires(input1) | input2.toInt), remaining)
    case lshiftPattern(input, shift, output) :: t if wires contains input =>
      getInitWires(t, wires.updated(output, wires(input) << shift.toInt), remaining)
    case rshiftPattern(input, shift, output) :: t if wires contains input =>
      getInitWires(t, wires.updated(output, wires(input) >> shift.toInt), remaining)
    case notPattern(input, output) :: t  if wires contains input =>
      getInitWires(t, wires.updated(output, 65536 + ~wires(input)), remaining)
    case h :: t => getInitWires(t, wires, h :: remaining)
  }

  val a: Int = loopWires(data, Map())("a")

  override def answer1(): Any = a
    
  override def answer2(): Any =
    loopWires(data.filterNot(_.endsWith(" -> b")), Map("b" -> a))("a")
}
package aoc.aoc2021

import aoc.Day

import scala.annotation.tailrec

val hexToBin = Map('0' -> List(0, 0, 0, 0), '1' -> List(0, 0, 0, 1),
  '2' -> List(0, 0, 1, 0), '3' -> List(0, 0, 1, 1),
  '4' -> List(0, 1, 0, 0), '5' -> List(0, 1, 0, 1),
  '6' -> List(0, 1, 1, 0), '7' -> List(0, 1, 1, 1),
  '8' -> List(1, 0, 0, 0), '9' -> List(1, 0, 0, 1),
  'A' -> List(1, 0, 1, 0), 'B' -> List(1, 0, 1, 1),
  'C' -> List(1, 1, 0, 0), 'D' -> List(1, 1, 0, 1),
  'E' -> List(1, 1, 1, 0), 'F' -> List(1, 1, 1, 1))


sealed trait Packet {
  val version: BigInt
  val id: BigInt
  val value: BigInt
  val remaining: List[Int]
  def versionSum(): BigInt
}

case class Literal(version: BigInt, id: BigInt, data: List[Int]) extends Packet {

  override def versionSum(): BigInt = version

  @tailrec
  private def getVal(remData: List[Int], currVals: List[Int] = List()): (BigInt, List[Int]) = remData match {
    case rd if rd.head == 0 =>
      val (n, rem) = rd.splitAt(5)
      (l2b(currVals ++ n.tail), rem)
    case rd =>
      val (n, rem) = rd.splitAt(5)
      getVal(rem, currVals ++ n.tail)
  }

  val (value, remaining) = getVal(data)
}

case class Operator(version: BigInt, id: BigInt, data: List[Int]) extends Packet {

  private val mode :: remainingData = data

  private def f(n: Int): (BigInt, List[Int]) =
    val (lenBits, remainingData2) = remainingData.splitAt(n)
    (l2b(lenBits), remainingData2)

  private val (packetSize, remainingData2) = mode match {
    case 0 => f(15)
    case 1 => f(11)
  }

  @tailrec
  private def getSubPackets1(data: List[Int], subpackets: List[Packet] = List()): (List[Packet], List[Int]) = subpackets match {
    case subpacks if subpacks.size == packetSize => (subpacks, data)
    case _ =>
      val p = getPacket(data)
      getSubPackets1(p.remaining, subpackets :+ p)
  }

  @tailrec
  private def getSubPackets0(data: List[Int], subpackets: List[Packet] = List()): List[Packet] = data match {
    case d if d.sum == 0 => subpackets
    case _ =>
      val p = getPacket(data)
      getSubPackets0(p.remaining, subpackets :+ p)
  }

  val (subpackets: List[Packet], remaining: List[Int]) = mode match {
    case 0 =>
      val (forFunction, rem) = remainingData2.splitAt(packetSize.toInt)
      (getSubPackets0(forFunction), rem)
    case 1 => getSubPackets1(remainingData2)
  }

  def versionSum(): BigInt = subpackets.foldLeft(version)((v, pack) => v + pack.versionSum())

  val value: BigInt = id match {
    case 0 => subpackets.foldLeft(BigInt(0))((v, pack) => v + pack.value)
    case 1 => subpackets.foldLeft(BigInt(1))((v, pack) => v * pack.value)
    case 2 => subpackets.map(_.value).min
    case 3 => subpackets.map(_.value).max
    case 5 => if subpackets.head.value > subpackets(1).value then 1 else 0
    case 6 => if subpackets.head.value < subpackets(1).value then 1 else 0
    case 7 => if subpackets.head.value == subpackets(1).value then 1 else 0
  }


}

def getPacket(data: List[Int]): Packet =
  val (vBits, remain) = data.splitAt(3)
  val (oBits, remain2) = remain.splitAt(3)
  val version = l2b(vBits)
  val id = l2b(oBits)
  id match {
    case 4 => Literal(version, id, remain2)
    case _ => Operator(version, id, remain2)
  }

def l2b(l: List[Int]): BigInt = BigInt(l.mkString, 2)

class Day16 extends Day(16, 2021) {

  private val binData: List[Int] = readData().getLines().toList.head.toCharArray.flatMap(hexToBin).toList

  val p = getPacket(binData)

  override def answer1(): Any =
    val p = getPacket(binData)
    p.versionSum()

  override def answer2(): Any = p.value
}
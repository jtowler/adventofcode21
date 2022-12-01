package aoc.aoc2021

import aoc.Day

type MyPair = (Char, Char)

case class Polymer(pairs: Map[MyPair, BigInt], counts: Map[Char, BigInt]) {

  def score(): BigInt = counts.maxBy(_._2)._2 - counts.minBy(_._2)._2

  def update(rules: Map[MyPair, (MyPair, MyPair)]): Polymer =
    val splitPairs: List[(List[MyPair], BigInt)] = pairs.map {
      (pair, count) =>
        val (pair1, pair2) = rules(pair)
        (List(pair1, pair2), count)
    }.toList
    val expandPairs: List[(MyPair, BigInt)] = for {
      (pairList, count) <- splitPairs
      pair <- pairList
    } yield (pair, count)
    val newPairs: Map[MyPair, BigInt] = expandPairs.groupBy(_._1).map { (pairKey, pairAndCount) =>
      pairKey -> pairAndCount.map(_._2).sum
    }.toMap
    val newCounts: List[(Char, BigInt)] = splitPairs.map {
      (pairList, count) => (pairList.head._2, count)
    }.toList
    val newerCounts: Map[Char, BigInt] = newCounts.groupBy(_._1).map { (char, countList) =>
      char -> countList.map(_._2).sum
    }.toMap
    val allKeys = counts.keys ++ newerCounts.keys
    val finalMap = allKeys.map { k =>
      k -> (counts.getOrElse(k, BigInt(0)) + newerCounts.getOrElse(k, BigInt(0)))
    }.toMap
    Polymer(newPairs, finalMap)
}


class Day14 extends Day(14, 2021) {

  private val data = readData().getLines().toList
  private val template = data.head
  private val countChars: Map[Char, BigInt] = template.groupBy(identity).map { (c, s) =>
    c -> BigInt(s.length)
  }.toMap
  private val templateMap: Map[MyPair, BigInt] = (template.init zip template.tail).groupBy(identity).map {
    (c, l) => c -> BigInt(l.size)
  }.toMap
  private val rulePattern = """([A-Z])([A-Z]) -> ([A-Z])""".r
  private val ruleMap: Map[MyPair, (MyPair, MyPair)] = data.slice(2, data.size).map { x =>
    val rulePattern(i1, i2, out) = x
    (i1.head, i2.head) -> ((i1.head, out.head), (out.head, i2.head))
  }.toMap

  def loop(i: Int, polymers: List[Polymer]): List[Polymer] = i match {
    case n if n == 40 => polymers
    case _ => loop(i + 1, polymers.head.update(ruleMap) :: polymers)
  }
  private val allPolys = loop(0, List(Polymer(templateMap, countChars))).map(_.score())

  override def answer1(): Any = allPolys(allPolys.size - 11)

  override def answer2(): Any = allPolys.head

}
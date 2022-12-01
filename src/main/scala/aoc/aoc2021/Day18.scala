package aoc.aoc2021

import aoc.Day

import java.lang.RuntimeException
import scala.annotation.tailrec

sealed trait SnailNumber {
  val path: List[Boolean]

  def getMag: Int

  def increment(direction: Boolean): SnailNumber = this match {
    case Leaf(value, path) => Leaf(value, direction :: path)
    case Split(left, right, path) => Split(left.increment(direction), right.increment(direction), direction :: path)
  }

  def +(that: SnailNumber): Split = Split(this.increment(true), that.increment(false), path)

  @tailrec
  final def getFromPath(path: List[Boolean]): SnailNumber = (this, path) match {
    case (s, Nil) => s
    case (p: Split, true :: t) => p.left.getFromPath(t)
    case (p: Split, false :: t) => p.right.getFromPath(t)
    case _ => throw RuntimeException("Malformed Path")
  }
}

case class Leaf(value: Int, path: List[Boolean] = List()) extends SnailNumber {
  override def toString: String = s"$value"

  def +(that: Int): Leaf = Leaf(value + that, path)

  def getMag: Int = value

  def split: Split =
    val n1 = value / 2
    val n2 = if value % 2 == 0 then n1 else n1 + 1
    Split(Leaf(n1, path :+ true), Leaf(n2, path :+ false), path)
}

case class Split(left: SnailNumber, right: SnailNumber, path: List[Boolean] = List()) extends SnailNumber {
  override def toString: String = s"[$left,$right]"

  def getMag: Int = 3 * left.getMag + 2 * right.getMag

  def get4Deep: Option[Split] = (left, right) match {
    case (l: Leaf, r: Leaf) if path.size >= 4 => Some(this)
    case (p: Split, _) if p.get4Deep.isDefined => p.get4Deep
    case (_, p: Split) if p.get4Deep.isDefined => p.get4Deep
    case _ => None
  }

  def get10: Option[Leaf] = (left, right) match {
    case (l: Leaf, _) if l.value >= 10 => Some(l)
    case (p: Split, _) if p.get10.isDefined => p.get10
    case (_, r: Leaf) if r.value >= 10 => Some(r)
    case (_, p: Split) if p.get10.isDefined => p.get10
    case _ => None
  }

  @tailrec
  final def reduce: Split =
    get4Deep match {
      case Some(p) => explode(p).reduce
      case None => get10 match {
        case Some(l) => split(l).reduce
        case None => this
      }
    }

  def split(toSplit: Leaf): Split = replace(toSplit.split, toSplit.path).asInstanceOf[Split]

  def explode(toExplode: Split): Split =
    val (lVal: Int, rVal: Int) = (toExplode.left, toExplode.right) match {
      case (l: Leaf, r: Leaf) => (l.value, r.value)
      case _ => throw RuntimeException("Exploding non base Pair")
    }
    val replacedLeft: Split = getLeft(toExplode.path) match {
      case Some(leaf) => replace(leaf + lVal, leaf.path).asInstanceOf[Split]
      case None => this
    }
    val replacedRight: Split = getRight(toExplode.path) match {
      case Some(leaf) => replacedLeft.replace(leaf + rVal, leaf.path).asInstanceOf[Split]
      case None => replacedLeft
    }
    replacedRight.replace(Leaf(0, toExplode.path), toExplode.path).asInstanceOf[Split]

  def replace(replaceWith: SnailNumber, replacePath: List[Boolean]): SnailNumber = (left, right, replacePath) match {
    case (_, _, Nil) => replaceWith
    case (l: Leaf, r, true :: Nil) => Split(replaceWith, r, path)
    case (l, r: Leaf, false :: Nil) => Split(l, replaceWith, path)
    case (l: Split, r, true :: t) => Split(l.replace(replaceWith, t), r, path)
    case (l, r: Split, false :: t) => Split(l, r.replace(replaceWith, t), path)
    case _ => throw RuntimeException("Error in pathing")
  }

  def getLeft(middlePath: List[Boolean]): Option[Leaf] = middlePath match {
    case p if p.forall(_ == true) => None
    case p =>
      val stripped = (true :: p.reverse.dropWhile(_ == true).tail).reverse
      getFromPath(stripped) match {
        case l: Leaf => Some(l)
        case p: Split => Some(p.getRightMost)
      }
  }

  def getRight(middlePath: List[Boolean]): Option[Leaf] = middlePath match {
    case p if p.forall(_ == false) => None
    case p =>
      val stripped = (false :: p.reverse.dropWhile(_ == false).tail).reverse
      getFromPath(stripped) match {
        case l: Leaf => Some(l)
        case p: Split => Some(p.getLeftMost)
      }
  }

  @tailrec
  private def getLeftMost: Leaf = this.left match {
    case l: Leaf => l
    case p: Split => p.getLeftMost
  }

  @tailrec
  private def getRightMost: Leaf = this.right match {
    case l: Leaf => l
    case p: Split => p.getRightMost
  }
}

object Leaf {
  def fromS(string: String, path: List[Boolean] = List()): Leaf = Leaf(string.toInt, path)

}

object Split {
  def fromS(string: String, path: List[Boolean] = List()): Split =
    @tailrec
    def split(n: Int = 0, count: Int = 0): Int = (string(n), count) match {
      case (',', 1) => n
      case ('[', _) => split(n + 1, count + 1)
      case (']', _) => split(n + 1, count - 1)
      case _ => split(n + 1, count)
    }

    val n = split()
    val (l, r) = string.splitAt(n)
    val l2 = l.slice(1, l.length)
    val r2 = r.slice(1, r.length - 1)
    val lSN = if l2.forall(_.isDigit) then Leaf.fromS(l2, path :+ true) else Split.fromS(l2, path :+ true)
    val rSN = if r2.forall(_.isDigit) then Leaf.fromS(r2, path :+ false) else Split.fromS(r2, path :+ false)
    Split(lSN, rSN, path)

}

class Day18 extends Day(18, 2021) {

  val data: List[Split] = readData().getLines().map(s => Split.fromS(s).reduce).toList

  override def answer1(): Any = data.tail.foldLeft(data.head)((curr, next) => (curr + next).reduce).getMag

  override def answer2(): Any =
    (for {i <- data.indices
          j <- data.indices
          if i != j
          } yield (data(i) + data(j)).reduce.getMag).max
}
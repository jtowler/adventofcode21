import scala.annotation.tailrec

class Day9 extends Day(9) {

  private val data = readData().getLines().map(_.toCharArray.map(_.asDigit).toList).toList

  def isLow(x: Int, y: Int): Boolean = data(x)(y) < getAdjs(x, y).map((x,y) => data(x)(y)).min

  def getAdjs(x: Int, y: Int): List[(Int, Int)] =
    List((x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)).filter(isInBounds)

  def isInBounds(x: Int, y: Int): Boolean = x >= 0 && y >= 0 && y < data.head.size && x < data.size

  @tailrec
  private def getBasin(currBasin: List[(Int, Int)]): List[(Int, Int)] =
    val allNewAdj = currBasin.map { (curX, curY) =>
      getAdjs(curX, curY).filterNot(currBasin contains _)
        .filter((xa, ya) => data(xa)(ya) != 9)
    }.flatten.distinct
    if allNewAdj.size == 0 then
      currBasin.sorted
    else
      getBasin(currBasin ++ allNewAdj)


  private def answer[T](filtFun: (Int, Int) => Boolean, yieldFun: (Int, Int) => T): Seq[T] =
    for {
      x <- data.indices
      y <- data.head.indices
      if filtFun(x, y)
    } yield yieldFun(x, y)

  override def answer1(): Any = answer(isLow, (x, y) => data(x)(y) + 1).sum

  override def answer2(): Any =
    answer((x, y) => data(x)(y) != 9, (x, y) => getBasin(List((x, y))))
      .distinct.map(_.size).sorted.takeRight(3).product
}
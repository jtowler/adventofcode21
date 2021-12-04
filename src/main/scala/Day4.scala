class Board(board: List[List[Int]]) {

  def checkWon(numbers: List[Int]): Boolean =
    val rows = board.map(line => line.count(numbers contains _) == 5).max
    val cols = board.transpose.map(line => line.count(numbers contains _) == 5).max
    rows || cols

  def score(numbers: List[Int]): Int = (for
  (l <- board;
   i <- l if !(numbers contains i))
  yield i).sum * numbers.last

  override def toString(): String =
    board.map { line => line.mkString(", ") }.mkString("\n")
}

class Day4 extends Day(4) {

  val data = readData().getLines().toList
  val numbers = data(0).split(",").map(_.toInt).toList

  val boards = data.tail.grouped(6).map(_.tail).map {
    board =>
      Board(board.map {
        _.trim().replace("  ", " ").split(" ").map(_.toInt).toList
      }.toList)
  }.toList


  override def answer1(): Any =
    def loop(i: Int): Int =
      val currNumbers = numbers.slice(0, i)
      boards.find(_.checkWon(currNumbers)) match {
        case Some(board) => board.score(currNumbers)
        case None => loop(i + 1)
      }

    loop(0)

  override def answer2(): Any =
    def loop(bs: List[Board], i: Int): Int =
      val currNumbers = numbers.slice(0, i)
      if bs.size == 1 && bs(0).checkWon(currNumbers) then
        bs(0).score(currNumbers)
      else
        loop(bs.filterNot(_.checkWon(currNumbers)), i + 1)

    loop(boards, 0)
}
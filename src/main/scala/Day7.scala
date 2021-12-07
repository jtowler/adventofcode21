class Day7 extends Day(7) {

  val data = readData().getLines().toList(0).split(",").map(_.toInt).toList

  def fact(a: Int): Int = a match {
    case n if n <= 1 => n
    case n => n + fact(n - 1)
  }

  def answer(f: (Int, Int) => Int): Int = (data.min to data.max).map{i =>
    data.map(x => f(x, i)).sum
  }.min

  override def answer1(): Any = answer((x, y) => (x-y).abs)

  override def answer2(): Any = answer((x, y) => fact((x-y).abs))

}
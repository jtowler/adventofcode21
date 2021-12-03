class Day3 extends Day(3) {

  val data = readData().getLines().toList
  val numEntries = data.size
  val numLength = data(0).length

  def getDigit(pos: Int): Int =
    val b = data.map(x => x(pos)).count(_ == '1')
    if b > numEntries / 2 then 1 else 0

  override def answer1(): Any =

    val s = List.range(0, numLength).map(getDigit)
    val s2 = s.map(x => if x == 1 then 0 else 1)
    Integer.parseInt(s.mkString, 2) * Integer.parseInt(s2.mkString, 2)

  override def answer2(): Any =
    def loop(data: List[String], pos: Int, func: (Int, Int) => Boolean): Int = {
      if data.size == 1 then Integer.parseInt(data(0).mkString, 2)
      else {
        val ones = data.map(_(pos)).count(_ == '1')
        val zeros = data.map(_(pos)).count(_ == '0')
        val mostCommon = if func(ones, zeros) then '1' else '0'
        loop(data.filter(_(pos) == mostCommon), pos + 1, func)
      }
    }
    loop(data, 0, _ >= _) * loop(data, 0, _ < _)
}
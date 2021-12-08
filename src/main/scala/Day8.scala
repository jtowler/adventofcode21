class Day8 extends Day(8) {

  val data = readData().getLines().map(_.split(""" \| """)).toList
  val easy = data.map(_(1).split(" ").map(_.toCharArray.sorted.toList))
  val hard = data.map(_(0).split(" ").map(_.toCharArray.sorted.toList))

  override def answer1(): Any = easy.map(line => line.count(List(2, 3, 4, 7) contains _.size)).sum

  def determineVals(disps: Array[List[Char]]): Map[List[Char], Char] =
    val one = disps.filter(_.size == 2)(0)
    val four = disps.filter(_.size == 4)(0)
    val seven = disps.filter(_.size == 3)(0)
    val eight = disps.filter(_.size == 7)(0)

    val sixes = disps.filter(_.size == 6)
    val fives = disps.filter(_.size == 5)

    val three = fives.filter(disp => one.map(seg => disp.contains(seg)).min)(0)
    val six = sixes.filterNot(disp => one.map(seg => disp.contains(seg)).min)(0)
    val nine = sixes.filter(disp => disp.count(seg => four contains seg) == 4)(0)
    val zero = sixes.filterNot(disp => disp == six || disp == nine)(0)

    val five = fives.filter(disp => disp != three && disp.count(seg => four contains seg) == 3)(0)
    val two = fives.filterNot(disp => disp == five || disp == three)(0)

    Map(one -> '1', two -> '2', three -> '3', four -> '4', five -> '5', six -> '6',
      seven -> '7', eight -> '8', nine -> '9', zero -> '0')

  override def answer2(): Any =
    val disps = easy zip hard.map(determineVals)
    disps.map { (disp, seg) => disp.map { seg(_)}.mkString.toInt}.sum
}

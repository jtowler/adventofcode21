class Day8 extends Day(8) {

  private val data = readData().getLines().map(_.split(""" \| """)).toList
  private val easy = data.map(_(1).split(" ").map(_.toCharArray.sorted.toList))

  override def answer1(): Any = easy.map(line => line.count(List(2, 3, 4, 7) contains _.size)).sum

  def determineVals(disps: Array[List[Char]]): Map[List[Char], Char] =
    val one = disps.find(_.size == 2).get
    val four = disps.find(_.size == 4).get
    val seven = disps.find(_.size == 3).get
    val eight = disps.find(_.size == 7).get

    val sixes = disps.filter(_.size == 6)
    val fives = disps.filter(_.size == 5)

    val three = fives.find(disp => one.map(disp contains _).min).get
    val six = sixes.find(disp => !one.map(disp contains _).min).get
    val nine = sixes.find(disp => disp.count(four contains _) == 4).get
    val zero = sixes.find(disp => disp != six && disp != nine).get

    val five = fives.find(disp => disp != three && disp.count(four contains _) == 3).get
    val two = fives.find(disp => disp != five && disp != three).get

    Map(one -> '1', two -> '2', three -> '3', four -> '4', five -> '5', six -> '6',
      seven -> '7', eight -> '8', nine -> '9', zero -> '0')

  override def answer2(): Any =
    val hard = data.map(_(0).split(" ").map(_.toCharArray.sorted.toList))
    val disps = easy zip hard.map(determineVals)
    disps.map { (disp, seg) => disp.map { seg(_)}.mkString.toInt}.sum
}

class Day1 extends Day(1) {

  val depths = readData().getLines().map(_.toInt).toList

  override def answer1(): Any = depths.tail.zip(depths.init).count{ _ > _ }

  override def answer2(): Any =
    val window = depths.sliding(3).map(_.sum).toList
    window.tail.zip(window.init).count{ _ > _ }
}
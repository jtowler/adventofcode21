package aoc.aoc2021

import aoc.Day

import scala.annotation.tailrec

case class Scanner(id: Int, beacons: List[Beacon]) {

  def permutations: List[Scanner] =
    val bs: List[List[Beacon]] = beacons.map(_.permutations)
    val maxLen = bs.map(_.size).max
    val bs2 = bs.map(b => b.padTo(maxLen, b.head))
    bs2.transpose.map(b => Scanner(id, b.distinct))

  def countOverlap(that: Scanner): Int = that.beacons.count(beacons contains _)

  def overlap(that: Scanner): Scanner = Scanner(id, (beacons ++ that.beacons).distinct)

}


case class Beacon(x: Int, y: Int, z: Int) {
  def permutations: List[Beacon] =
    val dirs = for {
      a <- List(-x, x)
      b <- List(-y, y)
      c <- List(-z, z)
    } yield List(a, b, c)
    dirs.flatMap(_.permutations).map(l => Beacon(l.head, l(1), l(2)))

  def alignWithCentre(scanner: Scanner, that: Beacon, thatId: Int): (Scanner, Beacon) =
    val xOffset = x - that.x
    val yOffset = y - that.y
    val zOffset = z - that.z
    val alignedScanner = Scanner(thatId, scanner.beacons.map(b => Beacon(b.x + xOffset, b.y + yOffset, b.z + zOffset)))
    val scannerCenter = Beacon(xOffset, yOffset, zOffset)
    (alignedScanner, scannerCenter)

  def manhattan(that: Beacon): Int = (x - that.x).abs + (y - that.y).abs + (z - that.z).abs
}

class Day19 extends Day(19, 2021) {

  private val scannerPattern = """--- scanner (\d+) ---""".r
  private val beaconPattern = """(-?\d+),(-?\d+),(-?\d+)""".r

  def createScanners(lines: List[String], id: Int = 0, beacons: List[Beacon] = List(),
                     scanners: List[Scanner] = List()): List[Scanner] = lines match {
    case Nil => Scanner(id, beacons) :: scanners
    case scannerPattern(newId) :: t => createScanners(t, newId.toInt, List(), Scanner(id, beacons) :: scanners)
    case beaconPattern(x, y, z) :: t => createScanners(t, id, Beacon(x.toInt, y.toInt, z.toInt) :: beacons, scanners)
    case h :: t => createScanners(t, id, beacons, scanners)
  }

  def getMaxOverlapScannerWithAlignment(scanner0: Scanner, scanners: List[Scanner]): (Scanner, Beacon, Int) =
    val idsScannersCounts: List[(Int, (Scanner, Beacon, Int))] = scanners.map(scanner => scanner.id -> getMaxOverlapOrientationWithAlignment(scanner0, scanner))
    val (id, (bestScanner, scannerCenter, count)) = idsScannersCounts.maxBy((id, sc) => sc._3)
    (bestScanner, scannerCenter, id)

  def getMaxOverlapOrientationWithAlignment(scanner0: Scanner, nextScanner: Scanner): (Scanner, Beacon, Int) =
    val (maxOverlap, center) = (for {
      perm <- nextScanner.permutations
      thatBeacon <- perm.beacons
      thisBeacon <- scanner0.beacons
      aligned = thisBeacon.alignWithCentre(perm, thatBeacon, nextScanner.id)
    } yield aligned).maxBy((scanner, center) => scanner.countOverlap(scanner0))
    (scanner0.overlap(maxOverlap), center, maxOverlap.countOverlap(scanner0))


  @tailrec
  final def getBeaconsAndScanners(scanner0: Scanner, remainingScanners: List[Scanner], scannerCenters: List[Beacon] =
  List(Beacon(0, 0, 0))): (List[Beacon], List[Beacon]) = remainingScanners match {
    case Nil => (scanner0.beacons, scannerCenters)
    case remaining =>
      val (bestScanner, scannerCenter, bestId) = getMaxOverlapScannerWithAlignment(scanner0, remaining)
      val newScanner0 = scanner0.overlap(bestScanner)
      val newRemaining = remaining.filterNot(_.id == bestId)
      getBeaconsAndScanners(newScanner0, newRemaining, scannerCenter :: scannerCenters)
  }

  val data: List[String] = readData().getLines().toList
  val scanners: List[Scanner] = createScanners(data.tail).reverse
  private val (finalScanner, centers) = getBeaconsAndScanners(scanners.head, scanners.tail)

  override def answer1(): Any =  finalScanner.size

  override def answer2(): Any = (for {
    i <- centers
    j <- centers
  } yield i.manhattan(j)).max
}
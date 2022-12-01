package aoc.aoc2021

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import scala.util.Using
import scala.io.Source

class GridTest extends AnyFlatSpec with should.Matchers {

  private val testData = Using(Source.fromFile("resources/2021/day_20_test.txt")) { _.getLines().toList }.get

  val algorithm: List[Int] = testData.head.map(v => if v == '#' then 1 else 0).toList
  val inputImage: List[List[Int]] = testData.slice(2, testData.size).map(_.map(v => if v == '#' then 1 else 0).toList)

  "A Grid" should "correctly count integers" in {
    val testGrid: Grid = Grid(inputImage, identity)
    testGrid.sum shouldBe 10
  }

  it should "correctly return the value at requested coordinate" in {
    val testGrid: Grid = Grid(inputImage, identity)
    testGrid.getAtCoord(2, 2) shouldBe 0
    testGrid.getAtCoord(-1, -1) shouldBe 0
    Grid(inputImage, identity, 1).getAtCoord(-1, -1) shouldBe 1
  }

  it should "correctly calculate the value of the new coordinate" in {
    val testGrid: Grid = Grid(inputImage, identity)
    testGrid.getPixelAtCoord(2, 2, algorithm) shouldBe 1
  }

  it should "return a grid of appropriate size when updating" in {
    val testGrid: Grid = Grid(inputImage, identity).update(algorithm)
    testGrid.grid.size shouldBe inputImage.size + 2
    testGrid.grid.head.size shouldBe inputImage.head.size + 2
  }

  it should "return a grid of appropriate size after multiple updates" in {
    val testGrid: Grid = Grid(inputImage, identity).multiUpdate(algorithm, 2)
    testGrid.grid.size shouldBe inputImage.size + 4
    testGrid.grid.head.size shouldBe inputImage.head.size + 4
    testGrid.sum shouldBe 35
  }

}
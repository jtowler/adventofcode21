package aoc.aoc2021

import aoc.Day

case class Cave(name: String) {
  val isBig: Boolean = name.toUpperCase == name
}

class Day12 extends Day(12, 2021) {

  private val connections = readData().getLines().map(_.split("-").map(Cave.apply).toList).toList
  private val allConnection: List[List[Cave]] = (connections ++ connections.map(_.reverse)).distinct
  private val caves = connections.flatten.distinct
  private val connectionMap: Map[Cave, List[Cave]] = caves.map { cave =>
    cave -> allConnection.filter(_.head == cave).map(_.last)
  }.toMap

  override def answer1(): Any =

    def addToPath(currentLoc: Cave, currentPath: List[Cave]): Boolean =
      currentLoc.isBig | !(currentPath contains currentLoc)

    def loop(currentLoc: Cave, currentPath: List[Cave], completePaths: List[List[Cave]]): List[List[Cave]] =
      if currentLoc.name == "end" then (currentLoc :: currentPath) :: completePaths
      else if !addToPath(currentLoc, currentPath) then completePaths
      else
        val nextPaths = connectionMap(currentLoc)
        nextPaths.flatMap(cave => loop(cave, currentLoc :: currentPath, completePaths)).distinct


    loop(Cave("start"), List(), List()).size

  override def answer2(): Any =

    val smalls = caves.filterNot(_.isBig).filterNot(List("start", "end") contains _.name)

    def addToPath(currentLoc: Cave, currentPath: List[Cave], smallCave: Cave): Boolean =
      if currentLoc.isBig then true
      else if (currentLoc == smallCave) && (currentPath.count(_ == smallCave) < 2) then true
      else if !(currentPath contains currentLoc) then true
      else false

    def loop(currentLoc: Cave, currentPath: List[Cave],
             completePaths: List[List[Cave]], smallCave: Cave): List[List[Cave]] =
      if currentLoc.name == "end" then (currentLoc :: currentPath) :: completePaths
      else if !addToPath(currentLoc, currentPath, smallCave) then completePaths
      else
        val nextPaths = connectionMap(currentLoc)
        nextPaths.flatMap(cave => loop(cave, currentLoc :: currentPath, completePaths, smallCave)).distinct

    val allPaths: List[List[Cave]] = (Cave("nonexistent") :: smalls).flatMap { small =>
      loop(Cave("start"), List(), List(), small)
    }.distinct
    allPaths.size

}
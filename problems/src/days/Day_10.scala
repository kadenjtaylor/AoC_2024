package days

import model.Day
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import model.Utils

object Day_10 extends Day {

  private val exampleData = """0123
                              |1234
                              |8765
                              |9876""".stripMargin

  private val twoTrailsExample = """...0...
                                   |...1...
                                   |...2...
                                   |6543456
                                   |7.....7
                                   |8.....8
                                   |9.....9""".stripMargin

  private val largerExample = """89010123
                                |78121874
                                |87430965
                                |96549874
                                |45678903
                                |32019012
                                |01329801
                                |10456732""".stripMargin

  case class Location(r: Int, c: Int) {
    override def toString(): String = s"($r, $c)"
  }

  type Height = Int

  case class TopographicalMap(tiles: Map[Location, Height]) {
    def trailheads() = tiles
      .filter((loc, height) => height == 0)
      .map((loc, _) => loc)

    def stepsFrom(loc: Location) =
      tiles.get(loc) match
        case None => List()
        case Some(height) =>
          List(
            Location(loc.r + 1, loc.c),
            Location(loc.r - 1, loc.c),
            Location(loc.r, loc.c + 1),
            Location(loc.r, loc.c - 1)
          ).flatMap(loc =>
            tiles.get(loc) match
              case Some(nextheight) if nextheight - height == 1 => Some(loc)
              case _                                            => None
          )

    def extend(trail: List[Location]): List[List[Location]] =
      trail.lastOption match
        case None       => List()
        case Some(last) => stepsFrom(last).map(step => trail :+ step)

    def allTrails() =
      var trails = trailheads().map(List(_)).toList
      var done   = false
      Range(0, 9).foreach { _ =>
        trails = trails.flatMap(extend(_))
      }
      trails

    def trailheadScores() =
      allTrails()
        .groupBy(trail => trail.head)
        .map((loc, trails) => (loc, trails.groupBy(_.last).size))

    def trailheadRatings() =
      allTrails()
        .groupBy(trail => trail.head)
        .map((loc, trails) => (loc, trails.size))
  }

  object TopographicalMap {
    def parse(s: String) = {
      TopographicalMap(
        s.linesIterator.zipWithIndex
          .flatMap((line, r) =>
            line
              .toCharArray()
              .zipWithIndex
              .map((heightChar, c) => {
                val num = Try(heightChar.toString().toInt) match
                  case Failure(exception) => -1
                  case Success(value)     => value
                (Location(r, c), num)
              })
          )
          .toMap
      )
    }
  }

  override def example: Unit = {
    val terrain    = TopographicalMap.parse(largerExample)
    val trailheads = terrain.trailheads()
    val scoreMap   = terrain.trailheadScores()
    val total      = scoreMap.values.sum
    println(total)
  }

  override def part1: Unit = {
    val terrain    = TopographicalMap.parse(Utils.readDailyResourceIntoString(10))
    val trailheads = terrain.trailheads()
    val scoreMap   = terrain.trailheadScores()
    val total      = scoreMap.values.sum
    println(total)
  }

  override def part2: Unit = {
    val terrain    = TopographicalMap.parse(Utils.readDailyResourceIntoString(10))
    val trailheads = terrain.trailheads()
    val scoreMap   = terrain.trailheadRatings()
    val total      = scoreMap.values.sum
    println(total)
  }

}

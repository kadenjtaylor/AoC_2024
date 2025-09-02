package days

import model.Day
import model.Utils

object Day_8 extends Day {
  private def exampleData = """............
                              |........0...
                              |.....0......
                              |.......0....
                              |....0.......
                              |......A.....
                              |............
                              |............
                              |........A...
                              |.........A..
                              |............
                              |............""".stripMargin

  private val basicExample = """..........
                           |...#......
                           |..........
                           |....a.....
                           |..........
                           |.....a....
                           |..........
                           |......#...
                           |..........
                           |..........""".stripMargin

  case class Location(r: Int, c: Int) {
    override def toString(): String = s"($r, $c)"
    def minus(rDiff: Int, cDiff: Int) = {
      Location(r - rDiff, c - cDiff)
    }

    def plus(rDiff: Int, cDiff: Int) = {
      Location(r + rDiff, c + cDiff)
    }

    def scale(loc: Location): Option[Int] = {
      val rScale = loc.r / r
      val cScale = loc.c / c
      if rScale == cScale then
        Some(rScale)
      else
        None
    }
  }

  case class AntennaMap(nodes: Map[Char, Set[Location]], farCorner: Location)

  /*  */
  def antinodes(m: AntennaMap, char: Char, resonant: Boolean = false): Set[Location] = {
    m.nodes
      .getOrElse(char, Set())
      .toSeq
      .combinations(2)
      .flatMap(locs => antinodesFor(locs(0), locs(1), m.farCorner, resonant))
      .toSet
  }

  def antinodesFor(l1: Location, l2: Location, farCorner: Location, resonant: Boolean): List[Location] = {
    val rDiff = l2.r - l1.r
    val cDiff = l2.c - l1.c
    val back = List.unfold(l1)(loc => {
      if inBounds(loc, farCorner.r, farCorner.c) then
        val nextLocation: Location = loc.minus(rDiff, cDiff)
        Some(loc, nextLocation)
      else None
    })
    val forth = List.unfold(l2)(loc => {
      if inBounds(loc, farCorner.r, farCorner.c) then
        val nextLocation: Location = loc.plus(rDiff, cDiff)
        Some(loc, nextLocation)
      else None
    })
    if !resonant then
      (back ++ forth)
        .filter(loc => loc != l1 && loc != l2)
        .filter(loc => {
          val l1Dist = l1.minus(loc.r, loc.c)
          val l2Dist = l2.minus(loc.r, loc.c)
          l1Dist.scale(l2Dist) == Some(2) || l2Dist.scale(l1Dist) == Some(2)
        })
    else
      (back ++ forth)
  }


  def inBounds(loc: Location, rMax: Int, cMax: Int): Boolean =
    0 <= loc.r && loc.r <= rMax && 0 <= loc.c && loc.c <= cMax

  object Parsing {
    def parse(s: String): AntennaMap = {
      var maxRow = 0
      var maxCol = 0
      var m      = Map[Char, Set[Location]]()
      s.linesIterator.zipWithIndex.foreach((line, row) => {
        if row > maxRow then maxRow = row
        line
          .toCharArray()
          .zipWithIndex
          .foreach((char, col) => {
            if col > maxCol then maxCol = col
            if (char != '.') {
              m = m.updatedWith(char)(_ match {
                case None       => Some(Set(Location(row, col)))
                case Some(locs) => Some(locs + Location(row, col))
              })
            }
          })
      })
      AntennaMap(m, Location(maxRow, maxCol))
    }
  }

  override def example: Unit = {
    val m = Parsing.parse(exampleData)
    val ans = m.nodes.keySet.flatMap(c => antinodes(m, c)).toSet
    println(s"Found ${ans.size} unique antinodes")
  }

  override def part1: Unit = {
    val m = Parsing.parse(Utils.readDailyResourceIntoString(8))
    val ans = m.nodes.keySet.flatMap(c => antinodes(m, c)).toSet
    println(s"Found ${ans.size} unique antinodes")
  }

  override def part2: Unit = {
    val m = Parsing.parse(Utils.readDailyResourceIntoString(8))
    val ans = m.nodes.keySet.flatMap(c => antinodes(m, c, true)).toSet
    println(s"Found ${ans.size} unique antinodes")
  }
}

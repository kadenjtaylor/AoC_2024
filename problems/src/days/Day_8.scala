package days

import model.Day

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

  case class Location(r: Int, c: Int) {
    override def toString(): String = s"($r, $c)"
  }

  case class AntennaMap(nodes: Map[Char, Set[Location]], farCorner: Location)

  /*  */
  def antinodeRules(m: AntennaMap, char: Char): Set[Location] = {
    m.nodes
      .getOrElse(char, Set())
      .toSeq
      .combinations(2)
      .flatMap(locs => antinodesFor(locs(0), locs(1), m.farCorner))
      .toSet
  }

  def antinodesFor(l1: Location, l2: Location, farCorner: Location): List[Location] = {
    println(s"Antinodes for $l1 and $l2 within (0, 0) -> $farCorner")
    val rDiff = l2.r - l1.r
    val cDiff = l2.c - l1.c
    println(s"R-Slope $rDiff / C-Slope $cDiff")
    List()
  }

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
    println(s"Map from (0, 0) to ${m.farCorner}")
    m.nodes.foreach { (c, locs) =>
      println(s"'$c' : [${locs.mkString(", ")}]")
    }
    antinodeRules(m, '0')
  }
}

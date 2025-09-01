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

  case class AntennaMap(nodes: Map[Char, Set[Location]])

  object Parsing {
    def parse(s: String): AntennaMap = {
      var m = Map[Char, Set[Location]]()
      s.linesIterator.zipWithIndex.foreach((line, row) => {
        line
          .toCharArray()
          .zipWithIndex
          .foreach((char, col) => {
            if (char != '.') {
              m = m.updatedWith(char)(_ match {
                case None       => Some(Set(Location(row, col)))
                case Some(locs) => Some(locs + Location(row, col))
              })
            }
          })
      })
      AntennaMap(m)
    }
  }

  override def example: Unit = {
    val m = Parsing.parse(exampleData)
    m.nodes.foreach { (c, locs) =>
      println(s"'$c' : [${locs.mkString(", ")}]")
    }
  }
}

package days

import model.Day

case object Day_10 extends Day {

  private val exampleData = """0123
                              |1234
                              |8765
                              |9876""".stripMargin

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

    def allTrails() =
      var trails = trailheads().map(List(_)).toList
      var done     = false
      while (!done) {
        // TODO: Grow each list where possible (branching)
        // Stop condition is that each trail is terminated at a 9
      }
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
                (Location(r, c), heightChar.toString().toInt)
              })
          )
          .toMap
      )
    }
  }

  override def example: Unit = {
    val terrain    = TopographicalMap.parse(exampleData)
    val trailheads = terrain.trailheads()
    println(exampleData)
    println(s"trailheads at:")
    trailheads.foreach(loc =>
      println(s"Trailhead: $loc => [${terrain.stepsFrom(loc).mkString(", ")}]")
    )
  }

}

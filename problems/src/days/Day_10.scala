package days

import model.Day

case object Day_10 extends Day {

  private val exampleData = """0123
                              |1234
                              |8765
                              |9876""".stripMargin

  case class Location(r: Int, c: Int)

  type Height = Int

  case class TopographicalMap(tiles: Map[Location, Height]) {
    def trailheads() = tiles
      .filter((loc, height) => height == 0)
      .map((loc, _) => loc)
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
    println(s"trailheads at:")
    trailheads.foreach(println(_))
  }

}

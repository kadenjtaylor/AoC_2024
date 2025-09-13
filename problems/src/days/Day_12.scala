package days

import model.Day
import scala.collection.mutable.Map

case object Day_12 extends Day {

  private val tinyExample = """AAAA
                              |BBCD
                              |BBCC
                              |EEEC""".stripMargin

  private val nestedExample = """OOOOO
                                |OXOXO
                                |OOOOO
                                |OXOXO
                                |OOOOO""".stripMargin

  private val biggerExample = """RRRRIICCFF
                                |RRRRIICCCF
                                |VVRRRCCFFF
                                |VVRCCCJFFF
                                |VVVVCJJCFE
                                |VVIVCCJJEE
                                |VVIIICJJEE
                                |MIIIIIJJEE
                                |MIIISIJEEE
                                |MMMISSJEEE""".stripMargin

  case class Plot(r: Int, c: Int) {
    def borders(row: Int, col: Int): Boolean =
      Math.abs(row - r) + Math.abs(col - c) == 1

    override def toString(): String = s"($r, $c)"
  }

  case class Region(char: Char, plots: Set[Plot]) {

    def merge(other: Region): Option[Region] = {
      if (other.char != char) {
        None
      } else {
        Some(Region(char, plots ++ other.plots))
      }
    }

    def expand(row: Int, col: Int): Region = {
      Region(char, plots + Plot(row, col))
    }

    def borders(row: Int, col: Int): Boolean = {
      plots.map(p => p.borders(row, col)).reduce(_ || _)
    }

    def area: Int =
      plots.size

    def perimeter: Int = ???

    def price: Int = area * perimeter
  }

  def belongsTo(char: Char, row: Int, col: Int, regions: Map[Int, Region]): List[Int] = {
    regions
      .filter((i, r) => r.char == char)      // Same letter?
      .filter((i, r) => r.borders(row, col)) // Is connected?
      .map((i, _) => i)                      // Only need the id
      .toList
  }

  case class Farm(private val regions: Map[Int, Region]) {
    def size: Int =
      regions.map((_, r) => r.plots.size).sum

    def region(row: Int, col: Int): Option[Int] =
      regions
        .flatMap((i, r) => if r.plots.contains(Plot(row, col)) then Some(i) else None)
        .headOption

    override def toString(): String =
      regions
        .map { (i, r) =>
          s"${r.char}-$i: [${r.plots.mkString(", ")}]"
        }
        .mkString("\n")

    def price: Int = regions.map((_, r) => r.price).sum
  }

  object Farm {
    def parse(s: String): Farm = {
      // Note: Mutable Resources!!
      var regionNumber = 0
      val regions      = Map[Int, Region]()
      s.split("\n")
        .map(s => s.toCharArray())
        .zipWithIndex
        .flatMap((line, row) => line.zipWithIndex.map((char, col) => (char, row, col)))
        .foreach((char, row, col) => {
          val matchingRegions = belongsTo(char, row, col, regions)
          if (matchingRegions.isEmpty) {
            regions.put(regionNumber, Region(char, Set(Plot(row, col))))
            regionNumber += 1
          } else {
            matchingRegions
              .map(regions.remove(_))
              .reduce((optR1, optR2) =>
                for {
                  r1 <- optR1
                  r2 <- optR2
                  r  <- r1.merge(r2)
                } yield r
              )
              .map(r => r.expand(row, col))
              .foreach(r => {
                regions.put(regionNumber, r)
                regionNumber += 1
              })
          }
          //   println(
          //     s"($char, $row, $col) -> [${matchingRegions
          //         .mkString(", ")}] => [${regions.map((i, r) => s"${r.char}${r.plots.size}").mkString(", ")}]"
          //   )
        })
      Farm(regions)
    }
  }

  override def example: Unit = {
    val farm = Farm.parse(biggerExample)
    println(farm.price)
  }
}

package days

import model.Day
import scala.collection.mutable.Map
import model.Utils

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

  private val eShaped = """EEEEE
                          |EXXXX
                          |EEEEE
                          |EXXXX
                          |EEEEE""".stripMargin

  private val abba = """AAAAAA
                       |AAABBA
                       |AAABBA
                       |ABBAAA
                       |ABBAAA
                       |AAAAAA""".stripMargin

  trait EdgeAPI {
    def r: Int
    def c: Int
    def length: Int
    def isVertical: Boolean
    def merge(other: Edge): Option[Edge]
  }

  enum Direction:
    case Down, Right

  enum ExtraDir:
    case Down(left: Char, right: Char)
    case Right(above: Char, below: Char)

  trait Directed:
    def dir: Direction

  enum EdgeData extends Directed:
    case SingleSegment(ex: ExtraDir)
    case MultiLengthSegment(length: Int, d: Direction)

    def dir: Direction = this match
      case SingleSegment(ExtraDir.Down(_, _))  => Direction.Down
      case SingleSegment(ExtraDir.Right(_, _)) => Direction.Right
      case MultiLengthSegment(_, dir)          => dir

  import Direction.*

  case class Edge(r: Int, c: Int, data: EdgeData) {
    // Note: Does not handle overlapping
    def merge(other: Edge): Option[Edge] = {
      val result = (this, other) match
        // case (Edge(ar, ac, aLength, Down), Edge(br, bc, bLength, Down))
        //     if ac == bc && ar + aLength == br => {
        //   Some(Edge(ar, ac, aLength + bLength, Down))
        // }
        // case (Edge(ar, ac, aLength, Down), Edge(br, bc, bLength, Down))
        //     if ac == bc && br + bLength == ar => {
        //   Some(Edge(br, bc, aLength + bLength, Down))
        // }
        // case (Edge(ar, ac, aLength, Right), Edge(br, bc, bLength, Right))
        //     if ar == br && bc + bLength == ac => {
        //   Some(Edge(ar, bc, bLength + aLength, Right))
        // }
        // case (Edge(ar, ac, aLength, Right), Edge(br, bc, bLength, Right))
        //     if ar == br && ac + aLength == bc => {
        //   Some(Edge(ar, ac, bLength + aLength, Right))
        // }
        case _ => None
      // println(s"${dir}($r, $c) <> ${other.dir}(${other.r}, ${other.c}) => $result")
      result
    }
  }

  import Edge.*
  import EdgeData.*

  case class Plot(r: Int, c: Int) {
    def borders(row: Int, col: Int): Boolean =
      Math.abs(row - r) + Math.abs(col - c) == 1

    def up: Plot = Plot(r - 1, c)

    def down: Plot = Plot(r + 1, c)

    def left: Plot = Plot(r, c - 1)

    def right: Plot = Plot(r, c + 1)

    override def toString(): String =
      s"($r, $c)"
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

    def perimeter: Int =
      plots.toList
        .flatMap(p => List(p.up, p.down, p.left, p.right))
        .filter(p => !plots.contains(p))
        .size

    def price: Int = area * perimeter

    def priceDescription(): String =
      s"A region of $char plants with price $area * $perimeter = $price"
  }

  def belongsTo(char: Char, row: Int, col: Int, regions: Map[Int, Region]): List[Int] = {
    regions
      .filter((i, r) => r.char == char)      // Same letter?
      .filter((i, r) => r.borders(row, col)) // Is connected?
      .map((i, _) => i)                      // Only need the id
      .toList
  }

  case class Farm(regions: Map[Int, Region]) {
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

    def bulkPriceDescription(): String =
      regions
        .map((i, r) => {
          val rSides    = sides((r.char, i)).length
          val bulkPrice = r.area * rSides
          s"A region of ${r.char} plants with price ${r.area}area (area) * ${rSides} (sides) = ${bulkPrice}"
        })
        .mkString("\n") +
        "\n\n" + s"So it has a total price of ${bulkPrice()}"

    def bulkPrice(): Int = regions.map((i, r) => r.area * sides((r.char, i)).length).sum

    def upFrom(r: Int, c: Int): (Edge, Plot) =
      (Edge(r, c, SingleSegment(ExtraDir.Right(???, ???))), Plot(r - 1, c))

    def downFrom(r: Int, c: Int): (Edge, Plot) =
      (Edge(r + 1, c, SingleSegment(ExtraDir.Right(???, ???))), Plot(r + 1, c))

    def leftFrom(r: Int, c: Int): (Edge, Plot) =
      (Edge(r, c, SingleSegment(ExtraDir.Down(???, ???))), Plot(r, c - 1))

    def rightFrom(r: Int, c: Int): (Edge, Plot) =
      (Edge(r, c + 1, SingleSegment(ExtraDir.Down(???, ???))), Plot(r, c + 1))

    def edges: Map[Int, List[Edge]] =
      regions
        .map((i, r) => {
          val currentPlots = r.plots.toList
          val result = currentPlots
            .flatMap(p =>
              List(upFrom(p.r, p.c), downFrom(p.r, p.c), leftFrom(p.r, p.c), rightFrom(p.r, p.c))
            )
            .filter((_, p) => !currentPlots.contains(p))
            .map((e, p) => e)
          (i, result)
        })

    def sides: Map[(Char, Int), List[Edge]] = {
      def foldMerge(es: List[Edge]) = {
        val mergedSides = es.foldLeft((List[Edge](), None: Option[Edge]))((pkg, side) => {
          val (sides, next) = pkg
          (next, side) match
            case (None, s) => (sides, Some(s))
            case (Some(e1), e2) => {
              val mergeResult = e1.merge(e2)
              mergeResult match
                case Some(merged) => (sides, Some(merged))
                case None         => (sides :+ e1, Some(e2))
            }
        })
        mergedSides._1 ++ mergedSides._2
      }
      edges.map((i, rEdges) => {
        val resultSides = {
          val (vertical, horizontal) = rEdges.partition(e => e.data.dir == Direction.Down)
          var vertSides = vertical
            .groupBy(e => e.c)
            .map((i, es) => (i, foldMerge(es.sortBy(e => e.r))))
          val horizSides = horizontal
            .groupBy(e => e.r)
            .map((i, es) => (i, foldMerge(es.sortBy(e => e.c))))
          (vertSides.values.flatten ++ horizSides.values.flatten).toList
        }
        ((regions(i).char, i), resultSides)
      })
    }

    def priceDescription(): String =
      regions.map((i, r) => s"- ${r.priceDescription()}").mkString("\n") +
        "\n\n" + s"So it has a total price of $price"
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

  override def part1: Unit =
    val farm = Farm.parse(Utils.readDailyResourceIntoString(12))
    println(farm.price)

  override def part2: Unit =
    println(abba)
    val farm = Farm.parse(abba)
    println(farm.bulkPriceDescription())
}

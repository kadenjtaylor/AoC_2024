package days

import model.Day
import scala.collection.mutable.Map
import model.Utils
import scala.collection.mutable.Queue

object Day_12 extends Day {

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

  private val abbaExample = """AAAAAA
                              |AAABBA
                              |AAABBA
                              |ABBAAA
                              |ABBAAA
                              |AAAAAA""".stripMargin

  private val largeEExample = """EEEEE
                                |EXXXX
                                |EEEEE
                                |EXXXX
                                |EEEEE""".stripMargin

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

    def edges() = {
      plots.toList
        .flatMap(p =>
          List(
            Edge(p.r, p.c, p.r + 1, p.c),         // left
            Edge(p.r, p.c, p.r, p.c + 1),         // top
            Edge(p.r, p.c + 1, p.r + 1, p.c + 1), // right
            Edge(p.r + 1, p.c, p.r + 1, p.c + 1)  // bottom
          )
        )
        .groupBy(e => e)
        .map((e, es) => (e, es.size))
        .filter((e, count) => count == 1)
        .keySet
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

    def priceDescription(): String =
      regions.map((i, r) => s"- ${r.priceDescription()}").mkString("\n") +
        "\n\n" + s"So it has a total price of $price"

    def orthoSplit(edgeMeetingPoint: (Int, Int)) =
      // println(s"Check Split for $edgeMeetingPoint")
      val (r, c) = edgeMeetingPoint
      val nw     = region(r - 1, c - 1)
      val ne     = region(r - 1, c)
      val sw     = region(r, c - 1)
      val se     = region(r, c)
      // println(s"$nw | $ne")
      // println(s"$sw | $se")
      // println(s"-----------------------")
      nw == sw || nw == ne || ne == se || se == sw
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

  // ======================================================================= //

  case class Edge(startR: Int, startC: Int, endR: Int, endC: Int) {
    def connectsAt(other: Edge): Option[(Int, Int)] =
      if (startR == other.endR && startC == other.endC) {
        Some((startR, startC)) // connected at my start point
      } else if (endR == other.startR && endC == other.startC) {
        Some((endR, endC)) // connected at my endpoint
      } else {
        None
      }

    def sameOrientation(other: Edge) =
      (startR == endR && other.startR == other.endR) ||
        (startC == endC && other.startC == other.endC)

    def isVertical = startC == endC

    def merge(other: Edge, farm: Farm): Option[Edge] = {
      if (sameOrientation(other)) {
        connectsAt(other) match
          case Some((connectR, connectC))
              if farm.orthoSplit(connectR, connectC) && connectR == startR && connectC == startC =>
            Some(Edge(other.startR, other.startC, endR, endC))
          case Some((connectR, connectC)) if farm.orthoSplit(connectR, connectC) =>
            Some(Edge(startR, startC, other.endR, other.endC))
          case _ => None

      } else {
        None
      }
    }
  }

  def cleave[A](l: List[A]): Option[(A, List[A])] = {
    if l.isEmpty then None
    else Some((l(0), l.tail))
  }

  def combine(farm: Farm, edges: Set[Edge]): Set[Edge] = {
    var done      = false
    var edgeNum   = 0
    var sides     = Map[Int, Edge]()
    val edgeQueue = Queue(edges.toSeq*)
    while (!edgeQueue.isEmpty) {
      val currentEdge = edgeQueue.dequeue()
      val (matches, others) = sides.partition((i, e) => {
        currentEdge.connectsAt(e) match
          case Some((r, c)) if currentEdge.sameOrientation(e) && farm.orthoSplit(r, c) => true
          case _                                                                       => false
      })
      if matches.isEmpty then
        // println(s"No matches - adding to map")
        sides.put(edgeNum, currentEdge)
        edgeNum += 1
      else if matches.size == 1 then
        // println(s"Exactly one match - merging the two under the existing key")
        matches.updateWith(matches.head._1)(_ match {
          case Some(matchingEdge) => currentEdge.merge(matchingEdge, farm)
          case _                  => None
        })
        sides = others ++ matches
      else
        // println(s"Multiple matches - merging into the first one, re-queuing the remainders")
        val (chosenMatch, rest) = matches.splitAt(1)
        chosenMatch.updateWith(matches.head._1)(_ match {
          case Some(matchingEdge) => currentEdge.merge(matchingEdge, farm)
          case _                  => None
        })
        sides = others ++ chosenMatch
        rest.values.foreach(bummer => edgeQueue.enqueue(bummer))
    }
    sides.values.toSet
  }

  def bulkPricing(f: Farm) = {
    var total = 0
    f.regions.foreach((i, r) => {
      val edges = r.edges()
      // println(s"Region ${r.char}-$i has ${edges.size} edges")
      val sides = combine(f, edges)
      // println(s"Region ${r.char}-$i has ${sides.size} sides")
      val bulkPrice = r.area * sides.size
      total += bulkPrice
      // println(s"Region ${r.char}-$i: ${r.area} area * ${sides.size} sides = $bulkPrice")
    })
    println(s"Total Bulk Price: $total")
  }
  override def part2: Unit = {
    def run(s: String) = {
      // println(s)
      val farm = Farm.parse(s)
      bulkPricing(farm)
    }
    run(Utils.readDailyResourceIntoString(12))
  }
}

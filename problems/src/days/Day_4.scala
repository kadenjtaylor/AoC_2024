package days

import model.Day
import scala.util.Try
import model.Utils

case object Day_4 extends Day {

  private val exampleData: String = """MMMSXXMASM
                                      |MSAMXMSMSA
                                      |AMXSXMAAMM
                                      |MSAMASMSMX
                                      |XMASAMXAMM
                                      |XXAMMXXAMA
                                      |SMSMSASXSS
                                      |SAXAMASAAA
                                      |MAMMMXMMMM
                                      |MXMXAXMASX""".stripMargin

  private def toCharGrid(s: String) =
    s.split("\n").map(_.toCharArray())

  case class WordSearchResult(row: Int, col: Int, dir: Direction)

  enum Direction:
    case Right, Left, Down, Up, UpRight, DownRight, DownLeft, UpLeft

  private def findTargetPhrase(
      grid: Array[Array[Char]],
      row: Int,
      col: Int,
      phrase: String,
      dir: Direction
  ): Option[WordSearchResult] = {
    val pattern = phrase
      .toCharArray()
      .zipWithIndex
    val advanceFn: (Char, Int) => Option[Char] = dir match
      case Direction.Right     => (c, i) => Try(grid(row)(col + i)).toOption
      case Direction.Left      => (c, i) => Try(grid(row)(col - i)).toOption
      case Direction.Up        => (c, i) => Try(grid(row - i)(col)).toOption
      case Direction.Down      => (c, i) => Try(grid(row + i)(col)).toOption
      case Direction.UpRight   => (c, i) => Try(grid(row - i)(col + i)).toOption
      case Direction.DownRight => (c, i) => Try(grid(row + i)(col + i)).toOption
      case Direction.DownLeft  => (c, i) => Try(grid(row + i)(col - i)).toOption
      case Direction.UpLeft    => (c, i) => Try(grid(row - i)(col - i)).toOption
    val target = pattern.flatMap((c, i) => advanceFn(c, i)).mkString
    if phrase == target then Some(WordSearchResult(row, col, dir))
    else None
  }

  private def search(
      grid: Array[Array[Char]],
      phrase: String,
      dirs: Array[Direction] = Direction.values
  ) =
    var wordsFound = 0
    val rows       = grid.length
    val cols       = grid(0).length
    val results = for {
      r <- Range(0, rows)
      c <- Range(0, cols)
      d <- dirs
    } yield findTargetPhrase(grid, r, c, phrase, d)
    results.flatten

  // ================================================= //

  override def example: Unit =
    val data     = exampleData
    val grid     = toCharGrid(data)
    val word     = "XMAS"
    val numFound = search(grid, word).length
    println(s"Found '$word' $numFound times")

  override def part1: Unit =
    val data     = Utils.readDailyResourceIntoString(4)
    val grid     = toCharGrid(data)
    val word     = "XMAS"
    val numFound = search(grid, word).length
    println(s"Found '$word' $numFound times")

  override def part2: Unit =
    val data = exampleData
    val grid = toCharGrid(data)
    val word = "MAS"
    val diagonals =
      Array(Direction.DownLeft, Direction.DownRight, Direction.UpLeft, Direction.UpRight)
    val results = search(grid, word, diagonals)
    // TODO: Fold the results into Xs that meet at the A
    // That means for each thing, figure out where the A is
    // Then determine if they overlap
    // Get the count of the Xs
    val numFound = 0
    println(s"Found '$word' $numFound times")

}

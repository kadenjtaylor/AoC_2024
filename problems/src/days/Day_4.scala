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
    // TODO: Just make Directions an apply method that transforms (r,c) pairs?
    val advanceFn: (Int) => Try[Char] = dir match
      case Direction.Right     => (i) => Try(grid(row)(col + i))
      case Direction.Left      => (i) => Try(grid(row)(col - i))
      case Direction.Up        => (i) => Try(grid(row - i)(col))
      case Direction.Down      => (i) => Try(grid(row + i)(col))
      case Direction.UpRight   => (i) => Try(grid(row - i)(col + i))
      case Direction.DownRight => (i) => Try(grid(row + i)(col + i))
      case Direction.DownLeft  => (i) => Try(grid(row + i)(col - i))
      case Direction.UpLeft    => (i) => Try(grid(row - i)(col - i))
    val target = Range(0, phrase.length()).flatMap(i => advanceFn(i).toOption).mkString
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

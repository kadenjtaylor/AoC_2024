package days

import model.Day
import scala.util.Try
import model.Utils

object Day_4 extends Day {

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

    def apply(r: Int, c: Int, i: Int): (Int, Int) =
      this match
        case Right     => (r, c + i)
        case Left      => (r, c - i)
        case Up        => (r - i, c)
        case Down      => (r + i, c)
        case UpRight   => (r - i, c + i)
        case DownRight => (r + i, c + i)
        case DownLeft  => (r + i, c - i)
        case UpLeft    => (r - i, c - i)

  private def findTargetPhrase(
      grid: Array[Array[Char]],
      row: Int,
      col: Int,
      phrase: String,
      dir: Direction
  ): Option[WordSearchResult] = {
    def charAt(i: Int): Option[Char] = {
      val (r, c) = dir(row, col, i)
      Try(grid(r)(c)).toOption
    }
    val target = Range(0, phrase.length()).flatMap(charAt(_)).mkString
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
    val data = Utils.readDailyResourceIntoString(4)
    val grid = toCharGrid(data)
    val word = "MAS"
    // Xs can only be made from diagonals, so we won't search ortho directions
    val diagonals =
      Array(Direction.DownLeft, Direction.DownRight, Direction.UpLeft, Direction.UpRight)
    val results = search(grid, word, diagonals)
    // An X-MAS only exists if two versions of the word cross through the same 'A'
    val numFound = results
      .groupBy(r => r.dir(r.row, r.col, 1)) // group by common 'A'
      .filter((k, v) => v.length > 1)       // get rid of the ones that don't have a pair
      .size
    println(s"Found '$word' $numFound times")

}

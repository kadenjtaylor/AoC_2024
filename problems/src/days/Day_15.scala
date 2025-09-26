package days

import model.Day
import model.Utils

object Day_15 extends Day {

  private val tinyExample = """########
                              |#..O.O.#
                              |##@.O..#
                              |#...O..#
                              |#.#.O..#
                              |#...O..#
                              |#......#
                              |########
                              |
                              |<^^>>>vv<v>>v<<""".stripMargin

  private val exampleData = """##########
                              |#..O..O.O#
                              |#......O.#
                              |#.OO..O.O#
                              |#..O@..O.#
                              |#O#..O...#
                              |#O..O..O.#
                              |#.OO.O.OO#
                              |#....O...#
                              |##########
                              |
                              |<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
                              |vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
                              |><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
                              |<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
                              |^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
                              |^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
                              |>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
                              |<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
                              |^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
                              |v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^""".stripMargin

  case class Pair(r: Int, c: Int) {
    def `+`(p: Pair): Pair = Pair(r + p.r, c + p.c)
    def gps                = (100 * r) + c
  }

  enum Move:
    case Up, Down, Left, Right

    def asPair = this match
      case Up    => Pair(-1, 0)
      case Down  => Pair(1, 0)
      case Left  => Pair(0, -1)
      case Right => Pair(0, 1)

    override def toString(): String = this match
      case Up    => "^"
      case Down  => "v"
      case Left  => "<"
      case Right => ">"

  case class Environment(robot: Pair, boxes: List[Pair], walls: List[Pair]) {

    override def toString(): String =
      val maxR = walls.map(p => p.r).max
      val maxC = walls.map(p => p.c).max
      (0 to maxR)
        .map(row =>
          (0 to maxC)
            .map(col => {
              val p = Pair(row, col)
              if boxes.contains(p) then "O"
              else if walls.contains(p) then "#"
              else if robot == p then "@"
              else "."
            })
            .mkString
        )
        .mkString(sep = "\n")

    def charAt(r: Int, c: Int): Option[Char] =
      val maxR = walls.map(p => p.r).max
      val maxC = walls.map(p => p.c).max
      if r >= 0 && r <= maxR && c >= 0 && c <= maxC then
        val p = Pair(r, c)
        if boxes.contains(p) then Some('O')
        else if walls.contains(p) then Some('#')
        else if robot == p then Some('@')
        else Some('.')
      else None

    def attempt(m: Move): Option[Environment] = {
      //   print(s"$m: ")
      val newLocation = robot + m.asPair
      if walls.contains(newLocation) then
        // println(s"Bumped into a wall - doing nothing.")
        None
      else if boxes.contains(newLocation) then
        // print(s"Found a box - ")
        var current = newLocation
        while (charAt(current.r, current.c) == Some('O')) {
          current = current + m.asPair
        }
        charAt(current.r, current.c) match
          case Some('.') => {
            // println(s"Shoving down to $current")
            val (toMove, others) = boxes.partition(p => p == newLocation)
            Some(Environment(newLocation, others :+ current, walls))
          }
          case _ => {
            // println(s"Can't shove from here - doing nothing")
            None
          }
      else
        // println(s"Found an empty space - moving to it")
        Some(Environment(newLocation, boxes, walls))
    }
  }

  object Environment {
    def parse(s: String) =
      var robotLocation: Option[Pair] = None
      var boxes: List[Pair]           = List()
      var walls: List[Pair]           = List()
      s.linesIterator.zipWithIndex.foreach((line, row) =>
        line
          .toCharArray()
          .zipWithIndex
          .foreach((char, col) => {
            char match
              case '@'     => robotLocation = Some(Pair(row, col))
              case 'O'     => boxes = boxes.appended(Pair(row, col))
              case '#'     => walls = walls.appended(Pair(row, col))
              case '.' | _ => ()
          })
      )
      Environment(robotLocation.get, boxes, walls)
  }

  object Moves {
    import fastparse.*, MultiLineWhitespace.*

    def left[$: P]    = P("<").map(_ => Move.Left)
    def right[$: P]   = P(">").map(_ => Move.Right)
    def up[$: P]      = P("^").map(_ => Move.Up)
    def down[$: P]    = P("v").map(_ => Move.Down)
    def anyMove[$: P] = P(left | right | up | down)
    def moves[$: P]   = P(anyMove).rep(1)

    def parse(s: String) =
      fastparse.parse(s, moves).get.value
  }

  def parse(s: String) =
    val parts = s.split("\n\n")
    val env   = Environment.parse(parts(0))
    val moves = Moves.parse(parts(1))
    (env, moves)

  def run(env: Environment, moves: Seq[Move]) =
    moves.foldLeft(env)((env, move) => {
      env.attempt(move) match
        case Some(nextEnv) => nextEnv
        case None          => env
    })

  override def example: Unit =
    val (env, moves) = parse(exampleData)
    println(s"Before:")
    println(env)
    val latestEnv = run(env, moves)
    println(s"After:")
    println(latestEnv)
    println(s"Sum of Coordinates: ${latestEnv.boxes.map(p => p.gps).sum}")

  override def part1: Unit =
    val (env, moves) = parse(Utils.readDailyResourceIntoString(15))
    println(s"Before:")
    println(env)
    val latestEnv = run(env, moves)
    println(s"After:")
    println(latestEnv)
    println(s"Sum of Coordinates: ${latestEnv.boxes.map(p => p.gps).sum}")
}

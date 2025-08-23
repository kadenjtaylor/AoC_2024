package days

import model.Day
import days.Day_6.Parsing.parseEnvironment
import scala.annotation.switch
import scala.util.Try
import model.Utils
import fastparse.internal.Util

case object Day_6 extends Day {

  private val exampleData = """....#.....
                              |.........#
                              |..........
                              |..#.......
                              |.......#..
                              |..........
                              |.#..^.....
                              |........#.
                              |#.........
                              |......#...""".stripMargin

  enum Direction:
    case Up, Down, Left, Right

    def apply(loc: Location): Location =
      val (r, c) = loc
      this match
        case Up    => (r - 1, c)
        case Down  => (r + 1, c)
        case Left  => (r, c - 1)
        case Right => (r, c + 1)

    def clockwise = this match
      case Up    => Right
      case Down  => Left
      case Left  => Up
      case Right => Down

  type Location = (Int, Int)

  case class Guard(loc: Location, dir: Direction) {
    def stepForward   = Guard(dir(loc), dir)
    def rotate90Right = Guard(loc, dir.clockwise)
  }

  enum Move:
    case Rotate90Right
    case StepForward

  enum Error:
    case Blocked
    case LoopDetected

  case class Environment(
      guard: Guard,
      obstacles: List[Location],
      visited: List[(Direction, Location)],
      bounds: (Int, Int)
  ) {
    def hasVisibleGuards: Boolean =
      (guard.loc, bounds) match
        case ((r, c), (rMax, cMax)) => {
          0 <= c && c < cMax && 0 <= r && r < rMax
        }

    def withObstable(loc: (Int, Int)): Either[Error, Environment] = {
      if guard.loc == loc || obstacles.contains(loc) then Left(Error.Blocked)
      else Right(Environment(guard, obstacles.appended(loc), visited, bounds))
    }

    def makeMove(m: Move): Either[Error, Environment] =
      val newGuard = m match
        case Move.Rotate90Right => Right(guard.rotate90Right)
        case Move.StepForward => {
          guard.stepForward match
            case Guard(loc, dir) if obstacles.contains(loc) => Left(Error.Blocked)
            case g @ Guard(loc, dir)                        => Right(g)
        }
      newGuard.flatMap(g =>
        if visited.contains((g.dir, g.loc)) then Left(Error.LoopDetected)
        else Right(Environment(g, obstacles, visited :+ (g.dir, g.loc), bounds))
      )

    def nextPhase(strategy: Strategy): Either[Error, Environment] =
      val move = strategy(this)
      makeMove(move)

    def get(r: Int, c: Int): Char =
      val gs = List(guard)
        .filter(_.loc == (r, c))
        .map(g =>
          g.dir match
            case Direction.Up    => '^'
            case Direction.Down  => 'V'
            case Direction.Left  => '<'
            case Direction.Right => '>'
        )
      val os      = obstacles.filter((oRow, oCol) => (oRow, oCol) == (r, c)).map(_ => '#')
      val vs      = visited.filter((_, loc) => (loc._1, loc._2) == (r, c)).map(_ => 'X')
      val empty   = List('.')
      val choices = (gs ++ os ++ vs ++ empty)
      choices.head

    def repr(): String =
      (for {
        (w, h) <- Seq(bounds)
        r      <- Range(0, w)
        c      <- Range(0, h)
        char = get(r, c)
        s    = if ((c + 1) % h == 0) then s"$char\n" else s"$char"
      } yield s).mkString

    def prettyPrint(): Unit =
      print(repr())
  }

  type Strategy = Environment => Move

  def basicStrat: Strategy = (env) => {
    val inFrontOfMe = env.guard.dir(env.guard.loc)
    if env.obstacles.contains(inFrontOfMe) then Move.Rotate90Right
    else Move.StepForward
  }

  object Parsing {

    case class EnvBuilder(
        guards: List[Guard],
        obstacles: List[Location]
    ) {
      def withGuard(g: Guard) = EnvBuilder(guards.appended(g), obstacles)

      def withObstacle(loc: Location) = EnvBuilder(guards, obstacles.appended(loc))

      // (Row X Column), or (Height X Width)
      def withBoundaryOf(originOppositeCorner: (Int, Int)) =
        Environment(guards.head, obstacles, List(), originOppositeCorner)
    }

    def parseEnvironment(s: String): Environment =
      val lines  = s.split("\n")
      val height = lines.length
      val width  = lines(0).length()
      lines.zipWithIndex
        .flatMap((row, rowIndex) =>
          row.zipWithIndex.map((char, colIndex) => (rowIndex, colIndex, char))
        )
        .foldLeft(EnvBuilder(List(), List()))((env, point) =>
          point match
            case (r, c, '#') => env.withObstacle((r, c))
            case (r, c, '^') => env.withGuard(Guard((r, c), Direction.Up))
            case (r, c, '>') => env.withGuard(Guard((r, c), Direction.Right))
            case (r, c, 'V') => env.withGuard(Guard((r, c), Direction.Down))
            case (r, c, '<') => env.withGuard(Guard((r, c), Direction.Left))
            case _           => env
        )
        .withBoundaryOf((height, width))
  }

  def guardPath(
      env: Environment,
      strategy: Strategy = basicStrat
  ): List[(Environment, Location)] =
    List.unfold(env)(inEnv =>
      inEnv
        .nextPhase(strategy)
        .flatMap(e => if e.hasVisibleGuards then Right(((inEnv, e.guard.loc), e)) else Left("FAIL"))
        .toOption
    )

  override def example: Unit =
    var env  = parseEnvironment(exampleData)
    val path = guardPath(env)
    println(path.toSet.size)

  override def part1: Unit =
    var env  = parseEnvironment(Utils.readDailyResourceIntoString(6))
    val path = guardPath(env)
    println(path.toSet.size)

  override def part2: Unit =
    var env = parseEnvironment(Utils.readDailyResourceIntoString(6))
    guardPath(env).foreach((env, point) =>
      println(s"Can putting an obstacle at $point cause a loop in {env}?")
    )

}

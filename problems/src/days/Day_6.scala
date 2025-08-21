package days

import model.Day
import days.Day_6.Parsing.parseEnvironment
import scala.annotation.switch
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import model.Utils
import java.util.Timer
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
    case Rotate90Right(i: Int)
    case StepForward(i: Int)

    def index = this match
      case Rotate90Right(index) => index
      case StepForward(index)   => index

  type Error = String

  def replaceWith[T](list: List[T])(index: Int, fn: T => Either[Error, T]): Either[Error, List[T]] =
    val replacementFailure = Left(s"Nothing to replace at index $index")
    if 0 <= index || index <= list.length then
      val (l1, l2) = list.splitAt(index)
      Try(l2.head).toEither.left
        .flatMap(_ => replacementFailure)
        .flatMap(fn)
        .map(el => (l1 :+ el) ++ l2.drop(1))
    else replacementFailure

  case class Environment(
      guards: List[Guard],
      obstacles: List[Location],
      visited: Set[Location],
      bounds: Option[(Int, Int)]
  ) {
    def hasVisibleGuards: Boolean = guards
      .map(g =>
        (bounds, g.loc) match
          case (Some(rMax, cMax), (r, c)) => c <= cMax && r <= rMax
          case (None, _)                  => true
      )
      .reduce(_ || _)

    def guard: Option[Guard] = guards.headOption

    def makeMove(m: Move): Either[Error, Environment] =
      val replacementFailure = Left(s"Nothing to replace at index ${m.index}}")
      if 0 <= m.index || m.index <= guards.length then
        val (l1, l2) = guards.splitAt(m.index)
        val thing: Either[String, (Guard, Option[Location])] = Try(l2.head).toEither.left
          .flatMap(_ => replacementFailure)
          .flatMap(guard =>
            m match
              case Move.Rotate90Right(i) => Right((guard.rotate90Right, None))
              case Move.StepForward(i) => {
                guard.stepForward match
                  case Guard(loc, dir) if obstacles.contains(loc) =>
                    Left("Guard can't go there - their path is blocked.")
                  case g @ Guard(loc, dir) =>
                    Right((g, Some(loc)))
              }
          )
        thing
          .map((el, loc) => ((l1 :+ el) ++ l2.drop(1), loc))
          .map((newGuards, visit) =>
            Environment(newGuards, obstacles, visited ++ visit.toList, bounds)
          )
      else replacementFailure

    def nextPhase(strategy: Strategy): Either[Error, Environment] =
      val move = strategy(this)
      makeMove(move)

    // (Row X Column), or (Height X Width)
    def withBoundaryOf(originOppositeCorner: (Int, Int)) =
      Environment(guards, obstacles, visited, Some(originOppositeCorner))

    def get(r: Int, c: Int): Char =
      val gs = guards
        .filter(_.loc == (r, c))
        .map(g =>
          g.dir match
            case Direction.Up    => '^'
            case Direction.Down  => 'V'
            case Direction.Left  => '<'
            case Direction.Right => '>'
        )
      val os      = obstacles.filter((oRow, oCol) => (oRow, oCol) == (r, c)).map(_ => '#')
      val vs      = visited.filter((vRow, vCol) => (vRow, vCol) == (r, c)).map(_ => 'X')
      val empty   = List('.')
      val choices = (gs ++ os ++ vs ++ empty)
      choices.head

    def repr(): String =
      (for {
        (w, h) <- bounds.toIndexedSeq
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
    env.guards.zipWithIndex
      .map((g, i) => {
        val inFrontOfMe = g.dir(g.loc)
        if env.obstacles.contains(inFrontOfMe) || env.guards.contains(inFrontOfMe) then
          Move.Rotate90Right(i)
        else Move.StepForward(i)
      })
      .head
  }

  object Parsing {
    def parseEnvironment(s: String): Environment =
      val lines  = s.split("\n")
      val height = lines.length
      val width  = lines(0).length()
      lines.zipWithIndex
        .flatMap((row, rowIndex) =>
          row.zipWithIndex.map((char, colIndex) => (rowIndex, colIndex, char))
        )
        .foldLeft(Environment(List(), List(), Set(), None))((env, point) =>
          point match
            case (r, c, '#') =>
              Environment(env.guards, env.obstacles.appended((r, c)), env.visited, env.bounds)
            case (r, c, '^') =>
              Environment(
                env.guards.appended(Guard((r, c), Direction.Up)),
                env.obstacles,
                env.visited,
                env.bounds
              )
            case (r, c, '>') =>
              Environment(
                env.guards.appended(Guard((r, c), Direction.Right)),
                env.obstacles,
                env.visited,
                env.bounds
              )
            case (r, c, 'V') =>
              Environment(
                env.guards.appended(Guard((r, c), Direction.Down)),
                env.obstacles,
                env.visited,
                env.bounds
              )
            case (r, c, '<') =>
              Environment(
                env.guards.appended(Guard((r, c), Direction.Left)),
                env.obstacles,
                env.visited,
                env.bounds
              )
            case _ =>
              Environment(
                env.guards,
                env.obstacles,
                env.visited,
                env.bounds
              ) // do nothing - empty case
        )
        .withBoundaryOf((height, width))
  }

  /*
    Returns the list of locations the guard would follow according to the
    given strategy, ending at the point the guard would exit the known
    environment.
   */
  def predictRoute(env: Environment, strategy: Strategy): (List[Location], Environment) =
    var e                    = env
    var done                 = false
    var moveNumber           = 0
    var locs: List[Location] = List()
    while (e.hasVisibleGuards && !done) {
      e.nextPhase(basicStrat) match
        case Left(err) => {
          println(err)
          done = true
        }
        case Right(newEnv) => {
          if newEnv.hasVisibleGuards then
            e.guard.foreach { g =>
              val l = g.loc
              locs = locs.appended(l)
            }
            e = newEnv
            moveNumber = moveNumber + 1
          else {
            done = true
          }
        }
    }
    (locs, e)

  override def example: Unit =
    var env             = parseEnvironment(exampleData)
    val (route, newEnv) = predictRoute(env, basicStrat)
    println(route.toSet.size)

  override def part1: Unit =
    var env             = parseEnvironment(Utils.readDailyResourceIntoString(6))
    val (route, newEnv) = predictRoute(env, basicStrat)
    println(route.toSet.size)

  override def part2: Unit = ()

}

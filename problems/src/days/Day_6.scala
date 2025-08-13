package days

import model.Day
import days.Day_6.Parsing.parseEnvironment
import scala.annotation.switch

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

  enum Element:
    case Guard(dir: Direction)
    case Obstacle
    case Empty

  type Environment = Array[Array[Element]]
  case class Location(row: Int, col: Int)

  enum Move:
    case Turn(dir: Direction)
    case StepForward

  type Strategy = Environment => Move

  def basicStrat: Strategy = (env) => {
    /* Guard Directions:
    - If there is something directly in front of you, turn right 90 degrees.
    - Otherwise, take a step forward.
     */
    ???
  }

  object Parsing {
    def parseEnvironment(s: String): Environment = ???
  }

  /*
    Returns the list of locations the guard would follow according to the
    given strategy, ending at the point the guard would exit the known
    environment.
   */
  def predictRoute(env: Environment, strategy: Strategy): List[Location] =
    ???

  override def example: Unit =
    val env     = parseEnvironment(exampleData)
    val squares = predictRoute(env, basicStrat).toSet.size
    println(s"Num unique sqaures: $squares")

  override def part1: Unit = ()

  override def part2: Unit = ()

}

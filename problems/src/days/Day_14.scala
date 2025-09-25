package days

import model.Day

/*
    Okay, I'm pretty sure what's going to happen here is that
    they want us to write a function f(An-1) -> An, then run it
    100 times to get the final result. But in the spirit of these
    puzzles, I'm sure the part 2 question is going to be: "Well
    what about 1 million steps from now???" It is at this point
    that we'll have to write a function g(n) -> An that, given a
    timestep number, will compute the positions of the robots AT
    THAT timestep. I think we're just gonna do that first.

    P.S. If I'm wrong and they change the game some other way
    (like adding back collisions) then this will suck.
 */

object Day_14 extends Day {

  private val exampleData = """p=0,4 v=3,-3
                              |p=6,3 v=-1,-3
                              |p=10,3 v=-1,2
                              |p=2,0 v=2,-1
                              |p=0,0 v=1,3
                              |p=3,0 v=-2,-2
                              |p=7,6 v=-1,-3
                              |p=3,0 v=-1,-2
                              |p=9,3 v=2,3
                              |p=7,3 v=-1,2
                              |p=2,4 v=2,-3
                              |p=9,5 v=-3,-3""".stripMargin

  private val singleRobot = "p=2,4 v=2,-3"

  case class Pair(r: Int, c: Int)

  private def wrap(n: Int, multiple: Int): Int =
    if n >= 0 && n < multiple then n
    else if n < 0 then
      val magicNumber: Int = ((0 - n) / multiple) + 1
      (n + (magicNumber * multiple)) % multiple
    else n % multiple

  case class Robot(position: Pair, velocity: Pair) {
    def positionAfter(seconds: Int, maxRows: Int, maxCols: Int): Pair = {
      val r = wrap(position.r + (velocity.r * seconds), maxRows)
      val c = wrap(position.c + (velocity.c * seconds), maxCols)
      Pair(r, c)
    }
  }

  object Robots {
    import fastparse.*, NoWhitespace.*

    def number[$: P]   = P(CharIn("0-9").rep(1).!.map(_.toInt))
    def positive[$: P] = number
    def negative[$: P] = P("-" ~ number).map(0 - _)
    def pair[$: P] =
      P((positive | negative) ~ "," ~ (positive | negative)).map((c, r) => Pair(r, c))
    def robot[$: P]  = P("p=" ~ pair ~ " v=" ~ pair).map((pos, vel) => Robot(pos, vel))
    def robots[$: P] = P(robot.rep(sep = "\n"))

    def parse(s: String) = fastparse.parse(s, robots).get.value
  }

  case class Environment(robots: Seq[Robot], rows: Int = 103, cols: Int = 101) {
    override def toString(): String =
      render()

    def render(quandrants: Boolean = false) =
      Range(0, rows)
        .map(row =>
          Range(0, cols)
            .map(col => {
              val botCount = robots.count(robot => robot.position == Pair(row, col))
              if quandrants && (row == (rows - 1) / 2 || col == (cols - 1) / 2) then " "
              else if botCount == 0 then "."
              else botCount.toString()
            })
            .mkString
        )
        .mkString("\n")

    def stateAfter(seconds: Int): Environment =
      Environment(
        robots.map(robot => Robot(robot.positionAfter(seconds, rows, cols), robot.velocity)),
        rows,
        cols
      )

    private def robotsIn(topLeft: Pair, bottomRight: Pair): Int =
      Range(topLeft.r, bottomRight.r)
        .flatMap(row =>
          Range(topLeft.c, bottomRight.c)
            .map(col => {
              robots.count(robot => robot.position == Pair(row, col))
            })
        )
        .sum

    def safetyFactor: Int =
      val halfRow = (rows - 1) / 2
      val halfCol = (cols - 1) / 2
      val nw      = robotsIn(Pair(0, 0), Pair(halfRow, halfCol))
      val ne      = robotsIn(Pair(0, halfCol + 1), Pair(halfRow, cols))
      val sw      = robotsIn(Pair(halfRow + 1, 0), Pair(rows, halfCol))
      val se      = robotsIn(Pair(halfRow + 1, halfCol + 1), Pair(rows, cols))
      println(s"NW: $nw / NE: $ne / SW: $sw / SE: $se")
      nw * ne * sw * se
  }

  override def example: Unit =
    val robots = Robots.parse(exampleData)
    // val fakeRobots = Range(0, 11).flatMap(c => Range(0, 7).map(r => Robot(Pair(r, c), Pair(0, 0))))
    val env     = Environment(robots, rows = 7, cols = 11)
    val updated = env.stateAfter(100)
    println(updated)
    println()
    println(updated.render(true))
    println(s"Safety Factor after 100 seconds: ${updated.safetyFactor}")

}

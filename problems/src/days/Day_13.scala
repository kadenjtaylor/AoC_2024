package days

import model.Day
import model.Utils

case object Day_13 extends Day {

  private val exampleData = """Button A: X+94, Y+34
                              |Button B: X+22, Y+67
                              |Prize: X=8400, Y=5400
                              |
                              |Button A: X+26, Y+66
                              |Button B: X+67, Y+21
                              |Prize: X=12748, Y=12176
                              |
                              |Button A: X+17, Y+86
                              |Button B: X+84, Y+37
                              |Prize: X=7870, Y=6450
                              |
                              |Button A: X+69, Y+23
                              |Button B: X+27, Y+71
                              |Prize: X=18641, Y=10279""".stripMargin

  case class Location(x: Int, y: Int)
  case class Button(x: Int, y: Int, cost: Int)

  case class Machine(a: Button, b: Button, prize: Location) {
    def equations =
      s"${a.x}a + ${b.x}b = ${prize.x}" + "\n" +
        s"${a.y}a + ${b.y}b = ${prize.y}"

    def dumbSolve(limit: Int = 100) = {
      (0 to limit)
        .flatMap(aChoice =>
          (0 to limit).flatMap(bChoice => {
            val xResult = aChoice * a.x + bChoice * b.x
            val yResult = aChoice * a.y + bChoice * b.y
            val solved  = xResult == prize.x && yResult == prize.y
            if solved then Some(aChoice * a.cost + bChoice * b.cost) else None
          })
        )
        .headOption
    }
  }

  object Parsing {
    import fastparse._, NoWhitespace._

    def number[$: P] = P(CharIn("0-9").rep(1).!.map(_.toInt))

    def negativeNumber[$: P] = P("-") ~ P(CharIn("0-9").rep(1).!.map(0 - _.toInt))

    def positiveNumber[$: P] = P("+") ~ number

    def amount[$: P] = negativeNumber | positiveNumber

    def coordinates[$: P] = P("X" ~ amount ~ ", " ~ "Y" ~ amount)

    def aButton[$: P] = P("Button A: " ~ coordinates).map((x, y) => Button(x, y, 3))

    def bButton[$: P] = P("Button B: " ~ coordinates).map((x, y) => Button(x, y, 1))

    def prizeLine[$: P] =
      P("Prize: X=" ~ number ~ ", " ~ "Y=" ~ number).map((x, y) => Location(x, y))

    def machine[$: P] =
      P(aButton ~ "\n" ~ bButton ~ "\n" ~ prizeLine).map((a, b, p) => Machine(a, b, p))

    def machines[$: P] = P(machine.rep(sep = "\n\n"))

    def parseData(s: String) =
      fastparse.parse(s, machines).get.value
  }

  override def example: Unit =
    val machines = Parsing.parseData(exampleData)
    val result   = machines.flatMap(m => m.dumbSolve()).sum
    println(s"Total Tokens Spent to win all prizes: $result")

  override def part1: Unit =
    val machines = Parsing.parseData(Utils.readDailyResourceIntoString(13))
    val result   = machines.flatMap(m => m.dumbSolve()).sum
    println(s"Total Tokens Spent to win all prizes: $result")
}

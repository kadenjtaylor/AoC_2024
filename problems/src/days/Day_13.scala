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

  case class Location(x: Long, y: Long)
  case class Button(x: Long, y: Long, cost: Int)

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

    private def printlnIfVerbose(s: String, verbose: Boolean = false) =
      if verbose then println(s)

    def smartSolve(): Option[Long] = {
      printlnIfVerbose(s"Need to solve the following system of equations:")
      printlnIfVerbose(equations)
      printlnIfVerbose(s"First, solve the second equation for a.")
      printlnIfVerbose(s"a = (${prize.y} - ${b.y}b) / ${a.y}")
      printlnIfVerbose(s"Then, solve the first equation for b.")
      printlnIfVerbose(s"b = (${prize.x} - ${a.x}a) / ${b.x}")
      printlnIfVerbose(s"Then substitute the value of b in the equation solved for a.")
      printlnIfVerbose(s"a = ${prize.y} - (${b.y} * ((${prize.x} - ${a.x}a) / ${b.x})) / ${a.y}")
      printlnIfVerbose(s"Then re-solve for a.")
      printlnIfVerbose(
        s"a = [(${prize.y}*${b.x}) - (${prize.x}*${b.y})] / [(${b.x}*${a.y}) - (${b.y}*${a.x})]"
      )
      val answerA = ((prize.y * b.x) - (prize.x * b.y)) / ((b.x * a.y) - (b.y * a.x))
      printlnIfVerbose(s"a = ${((prize.y * b.x) - (prize.x * b.y)) / ((b.x * a.y) - (b.y * a.x))}")
      printlnIfVerbose(s"Then plug that back into the b equation to solve for b.")
      printlnIfVerbose(s"b = (${prize.x} - ${a.x} * ${answerA}) / ${b.x}")
      val answerB = (prize.x - (a.x * answerA)) / b.x
      printlnIfVerbose(s"b = ${(prize.x - (a.x * answerA)) / b.x}")
      printlnIfVerbose(s"Our numbers are: a = $answerA, b = $answerB")
      val equation1Works = a.x * answerA + b.x * answerB == prize.x
      if !equation1Works then printlnIfVerbose(s"${a.x * answerA + b.x * answerB} != ${prize.x}")
      val equation2Works = a.y * answerA + b.y * answerB == prize.y
      if !equation2Works then printlnIfVerbose(s"${a.y * answerA + b.y * answerB} != ${prize.y}")
      val answerChecksOut = answerA > 0 && answerB > 0 && equation1Works && equation2Works
      if answerChecksOut then
        val tokens = answerA * a.cost + answerB * b.cost
        printlnIfVerbose(s"Therefore our token cost is: $answerA * 3 + $answerB * 1 = $tokens")
        Some(tokens)
      else None
    }

    def adjust(amount: Long = 10000000000000L) =
      Machine(a, b, Location(prize.x + amount, prize.y + amount))
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

  override def part2: Unit =
    val machines = Parsing
      .parseData(Utils.readDailyResourceIntoString(13))
      .map(m => m.adjust())
    val result = machines.flatMap(m => m.smartSolve()).sum
    println(s"Total Tokens Spent to win all prizes: $result")

}

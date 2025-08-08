import model.Day
import days.*
import scala.util.Try
import scala.util.Failure
import scala.util.Success

object Main {

  val days: List[Day] = List(
    Day_1,
    Day_2,
    Day_3
  )

  def main(args: Array[String]): Unit =
    if (args.length == 0) {
      val day = days.last
      runDay(day)
    } else if (args.length == 1) {

      if args(0) == "all" then days.foreach(runDay(_))
      else
        Try(args(0).toInt).map(i => days(i - 1)) match
          case Failure(exception) =>
            println(s"Failure to find day for input: ${args(0)}")
          case Success(day) =>
            runDay(day)
    } else {
      println("Unknown argument")
    }

  private def runDay(d: Day) =
    printGreen(
      s"${d.getClass().getName().stripPrefix("days.").stripSuffix("$")}:"
    )
    printMagenta(s"Example:")
    d.example
    println()
    printMagenta(s"Part 1:")
    d.part1
    println()
    printMagenta(s"Part 2:")
    d.part2
    println()

  private def printGreen(s: String) =
    println(Console.GREEN + s + Console.RESET)

  private def printMagenta(s: String) =
    println(Console.MAGENTA + s + Console.RESET)
}

import model.Day
import days.*

object Main {

  val days: List[Day] = List(
    Day1()
  )

  def main(args: Array[String]): Unit = runDays(days)

  private def runDays(days: List[Day]) =
    days.zipWithIndex.foreach { (d, i) =>
      println(Console.GREEN + s"Day $i:" + Console.RESET)
      println(Console.MAGENTA + s"Example:" + Console.RESET)
      d.example
      println()
      println(Console.MAGENTA + s"Part 1:" + Console.RESET)
      d.part1
      println()
      println(Console.MAGENTA + s"Part 2:" + Console.RESET)
      d.part2
      println()
    }

}

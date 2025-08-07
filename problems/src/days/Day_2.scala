package days

import model.Day
import scala.util.Try
import model.Utils
import days.Day_2.Step.Increasing
import days.Day_2.Step.Decreasing

case object Day_2 extends Day {

  private def exampleData: String = """7 6 4 2 1
                                      |1 2 7 8 9
                                      |9 7 6 2 1
                                      |1 3 2 4 5
                                      |8 6 4 4 1
                                      |1 3 6 7 9""".stripMargin

  enum Step:
    case Start
    case First(last: Int)
    case Increasing(last: Int)
    case Decreasing(last: Int)
    case Unsafe

  // =============================================================== //

  private def isSafe(report: Array[Int]) =
    report.foldLeft(Step.Start)((step, i) =>
      step match
        case Step.Start => Step.First(i)
        case Step.First(last) if i - last <= 3 && i - last >= 1 =>
          Increasing(i)
        case Step.First(last) if last - i <= 3 && last - i >= 1 =>
          Decreasing(i)
        case Step.Increasing(last) if i - last <= 3 && i - last >= 1 =>
          Increasing(i)
        case Step.Decreasing(last) if last - i <= 3 && last - i >= 1 =>
          Decreasing(i)
        case _ => Step.Unsafe
    ) match
      case Step.Unsafe => false
      case _           => true

  private def toInts(s: String) = s.split("\\s+").map(_.toInt)

  private def toLines(s: String) = s.split("\n")

  // =============================================================== //

  override def example: Unit =
    var numSafeReports = 0
    toLines(exampleData).foreach { line =>
      val report = toInts(line)
      val safe = isSafe(report)
      println(report.mkString(" ") + s", Safe: ${{ safe }}")
      if safe then numSafeReports += 1 else ()
    }
    println(s"There are $numSafeReports safe reports")

  override def part1 = ()

  override def part2 = ()

}

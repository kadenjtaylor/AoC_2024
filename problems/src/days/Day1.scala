package days

import model.Day
import scala.util.Try
import model.Utils

case class Day1() extends Day {

  private def exampleData: String = """3   4
                              |4   3
                              |2   5
                              |1   3
                              |3   9
                              |3   3
                              |""".stripMargin

  private def append(lists: (List[Int], List[Int]), line: String) =
    val nums = line.split("\\s+")
    val nextStep = for {
      n1 <- Try(nums(0).toInt)
      n2 <- Try(nums(1).toInt)
    } yield (lists._1.appended(n1), lists._2.appended(n2))
    nextStep.get

  override def example: Unit = {
    val (l1, l2) = exampleData.split("\n").foldLeft((List(), List()))(append)
    val runway = l1.sorted.zip(l2.sorted)
    var sum = 0
    runway.foreach((n1, n2) => {
      val diff = Math.abs(n2 - n1)
      println(s"$n1, $n2 => $diff")
      sum += diff
    })
    println(s"Total Diff: $sum")
  }

  override def part1 =
    val s = Utils.readDailyResourceIntoString(1)
    println(s)

  override def part2 = ()
}

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

  private def splitIntoNumberLists(s: String) =
    s.split("\n").foldLeft((List(), List()))(append)

  private def calculateSortedSumOfDiffs(
      numberLists: (List[Int], List[Int]),
      verbose: Boolean = false
  ) =
    val (l1, l2) = numberLists
    val runway = l1.sorted.zip(l2.sorted)
    var sum = 0
    runway.foreach((n1, n2) => {
      val diff = Math.abs(n2 - n1)
      if verbose then println(s"$n1, $n2 => $diff") else ()
      sum += diff
    })
    sum

  // =============================================================== //

  override def example: Unit = {
    val sum = calculateSortedSumOfDiffs(splitIntoNumberLists(exampleData))
    println(sum)
  }

  override def part1 =
    val s = Utils.readDailyResourceIntoString(1)
    val sum = calculateSortedSumOfDiffs(splitIntoNumberLists(s))
    println(sum)

  private def simpleWay(keys: List[Int], occurrences: List[Int]) =
    keys.map(k => (k, occurrences.filter(_ == k).length * k)).toMap

  override def part2 =
    val s = Utils.readDailyResourceIntoString(1)
    val (l1, l2) = splitIntoNumberLists(s)
    val counts = simpleWay(l1, l2)
    val sum = counts.values.sum
    println(sum)

}

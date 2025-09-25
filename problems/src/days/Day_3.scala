package days

import model.Day
import scala.util.matching.Regex.Match
import scala.util.Try
import scala.util.Failure
import scala.util.Success
import model.Utils

object Day_3 extends Day {

  private def exampleData: String =
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

  private def secondExample: String =
    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

  private val validMultInstruction = "mul\\((\\d+),(\\d+)\\)".r

  private val conditionals = "(do\\(\\)|don't\\(\\))".r

  private val combined = s"${validMultInstruction.pattern.pattern()}|${conditionals}".r

  private def sumMulInstructions(data: String, useConditionals: Boolean = false) =
    val results = (if useConditionals then combined else validMultInstruction).findAllIn(data)
    var total   = 0
    var enabled = true
    // println(s"Found ${results.length} matches.")
    results.matchData.foreach(m =>
      m match
        case validMultInstruction(firstNum, secondNum) =>
          // println(m.toString())
          val answer = for {
            a <- Try(firstNum.toInt)
            b <- Try(secondNum.toInt)
          } yield a * b
          answer match
            case Failure(exception) =>
              println(s"Couldn't perform ${m.toString()}")
            case Success(value) if enabled => total += value
            case Success(_)                => ()
        case conditionals("don't()") if useConditionals =>
          enabled = false
        case conditionals("do()") if useConditionals =>
          enabled = true
        case _ => ()
    )
    total

  override def example: Unit =
    val total = sumMulInstructions(exampleData)
    println(total)

  override def part1: Unit =
    val data  = Utils.readDailyResourceIntoString(3)
    val total = sumMulInstructions(data)
    println(total)

  override def part2: Unit =
    val data  = Utils.readDailyResourceIntoString(3)
    val total = sumMulInstructions(data, true)
    println(total)

}

package days

import model.Day
import scala.collection.mutable.ArrayBuffer
import scala.util.Try
import model.Utils

case object Day_11 extends Day {

  private val exampleData = "0 1 10 99 999"

  private val workedExample = "125 17"

  case class StoneLine(arr: ArrayBuffer[Long]) {
    override def toString(): String =
      s"[${arr.mkString(", ")}]"

    def blink(times: Int = 1): Unit =
      (1 to times).foreach { blinkNum =>
        var index   = 0
        var current = Try(arr(index))
        while (current.isSuccess) {
          val num = current.get
          if (num == 0) {
            // replace w/ 1
            arr.update(index, 1)
          } else if (num.toString.length() % 2 == 0) {
            // split the stones
            val digits          = num.toString()
            val (first, second) = digits.splitAt(digits.length() / 2)
            arr.update(index, first.toLong)
            index += 1
            arr.insert(index, second.toLong)
          } else {
            // multiply by 2024
            arr.update(index, num * 2024)
          }
          // adjust index
          index += 1
          // move pointer to new current stone
          current = Try(arr(index))
        }
        // println(s"After $blinkNum blink${if blinkNum == 1 then "" else "s"}:")
        // println(this.toString())
      }
  }

  object StoneLine {
    def parse(s: String): StoneLine =
      StoneLine(ArrayBuffer(s.split(" ").map(_.toLong)*))
  }

  override def example: Unit = {
    val stones = StoneLine.parse(workedExample)
    stones.blink(25)
    println(stones.arr.length)
  }

  override def part1: Unit = {
    val stones = StoneLine.parse(Utils.readDailyResourceIntoString(11))
    stones.blink(25)
    println(stones.arr.length)
  }
}

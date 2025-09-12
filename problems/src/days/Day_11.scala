package days

import model.Day
import scala.collection.mutable.ArrayBuffer
import scala.util.Try
import model.Utils
import scala.collection.mutable.Stack

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

  // ====================================================================== //

  import scala.collection.mutable.Map

  enum Frame:
    case Calculate(num: Long, times: Int)
    case Equals(in: (Long, Int), out: List[(Long, Int)])

  import days.Day_11.Frame.*

  override def part2: Unit =
    val times = 25

    val cheaterData = List(Calculate(10, 2))

    def oneStep(s: Stack[Frame], c: Map[(Long, Int), Long]): Unit = {
      val f = s.pop()
      f match
        case Calculate(num, 0) => {
          // println(s"($num, 0) -> 1")
          c.put((num, 0), 1)
        }
        case Calculate(0, times) => {
          // println(s"(0, $times) -> (1, ${times - 1})")
          s.push(Equals((0, times), List((1, times - 1))))
          s.push(Calculate(1, times - 1))
        }
        case Calculate(num, times) => {
          if (num.toString().length() % 2 == 0) {
            val digits = num.toString()
            val (l, r) = digits.splitAt(digits.length() / 2)
            // println(s"($num, $times) -> [($l, ${times - 1}), ($r, ${times - 1})]")
            s.push(Equals((num, times), List((l.toLong, times - 1), (r.toLong, times - 1))))
            s.push(Calculate(l.toLong, times - 1))
            s.push(Calculate(r.toLong, times - 1))
          } else {
            // println(s"($num, $times) -> (${num * 2024}, ${times - 1})")
            s.push(Equals((num, times), List((num * 2024, times - 1))))
            s.push(Calculate(num * 2024, times - 1))
          }
        }
        case Equals(in, out) => {
          // println(s"$in = ${out.mkString(" + ")}")
          val total = out.map(el => c(el)).sum
          c.put(in, total)
        }
    }

    val m = Map[(Long, Int), Long]()
    val data = Utils
      .readDailyResourceIntoString(11)
      .split(" ")
      .map(n => (n.toLong, 35))
    // println(s"Beginning with:  ${data.mkString(", ")}")

    val s = Stack[Frame]()
    s.push(Equals((-1L, -1), data.toList))
    data.reverse.foreach((num, times) => s.push(Calculate(num, times)))

    while (!s.isEmpty) {
      oneStep(s, m)
    }
    println(m.get((-1, -1)).get)

}

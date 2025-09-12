package days

import model.Day
import scala.collection.mutable.ArrayBuffer
import scala.util.Try
import model.Utils
import scala.collection.mutable.Queue
import scala.collection.mutable.Map
import scala.annotation.tailrec

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

  // ======================================================================= //

  def updates(num: Long): (Long, Long) | Long =
    if (num == 0) {
      1L
    } else if (num.toString().length() % 2 == 0) {
      val digits          = num.toString()
      val (first, second) = digits.splitAt(digits.length() / 2)
      (first.toLong, second.toLong)
    } else {
      num * 2024
    }

  def rip(data: Array[(Long, Int)]) = {
    // Mutable resources
    val q     = Queue(data*)
    val cache = Map[Long, Long | (Long, Long)]()
    var count = 0L

    q.dequeueWhile { (num, times) =>
      if (times == 0) {
        count = count + 1
      } else {
        cache.get(num) match
          case Some((left: Long, right: Long)) => {
            q.enqueue((left, times - 1))
            q.enqueue((right, times - 1))
          }
          case Some(n: Long) => {
            q.enqueue((n, times - 1))
          }
          case None => {
            val us = updates(num)
            cache.put(num, us)
            us match
              case (a: Long, b: Long) => {
                q.enqueue((a, times - 1))
                q.enqueue((b, times - 1))
              }
              case i: Long => {
                q.enqueue((i, times - 1))
              }
          }
      }

      true
    }
    count
  }

  override def part2: Unit = {
    val data = Utils
      .readDailyResourceIntoString(11)
      .split(" ")
      .map(n => (n.toLong, 25))

    val c = rip(data)
    println(c)
  }
}

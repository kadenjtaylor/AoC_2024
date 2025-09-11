package days

import model.Day
import scala.collection.mutable.ArrayBuffer
import scala.util.Try
import model.Utils
import cats.effect.IO
import fs2.{Chunk, INothing, Pipe, Pull, Pure, Stream}
import fs2.concurrent.Channel
import cats.effect.kernel.Ref

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
            val digits = num.toString()
            // val numDigits = Math.log10(num).toLong + 1
            val (first, second) = digits.splitAt(digits.length() / 2)
            // val first2 =
            //   (num / Math.pow(10, Math.floor(Math.log10(num)) - (numDigits / 2) + 1)).toLong
            // val second2 = num - (first2 * Math.pow(10, numDigits / 2)).toLong
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

        // println(s"After $blinkNum blink${if blinkNum == 1 then "" else "s"}: ${arr.length}")
        // println(this.toString())
      }
    def split(): ArrayBuffer[StoneLine] =
      arr.map(l => StoneLine(ArrayBuffer(l)))
  }

  object StoneLine {
    def parse(s: String): StoneLine =
      StoneLine(ArrayBuffer(s.split(" ").map(_.toLong)*))
  }

  override def example: Unit = {
    ()
    // val stones = StoneLine.parse(workedExample)
    // stones.blink(25)
    // println(stones.arr.length)
  }

  override def part1: Unit = {
    val stones = StoneLine.parse(Utils.readDailyResourceIntoString(11))
    stones.blink(25)
    println(stones.arr.length)
  }

  // ===================================================================================== //

  def applyRule(data: (Long, Int)): Stream[IO, Option[(Long, Int)]] =
    val (num, amount) = data
    if amount < 0 then Stream.eval(IO.println("NEGATIVE AMOUNT")) >> Stream(None)
    else if (num == 0) {
      Stream(Some((1L, amount - 1)))
    } else if (num.toString.length() % 2 == 0) {
      val digits          = num.toString()
      val (first, second) = digits.splitAt(digits.length() / 2)
      Stream(Some((first.toLong, amount - 1)), Some((second.toLong, amount - 1)))
    } else {
      Stream(Some((num * 2024, amount - 1)))
    }

  import cats.effect.unsafe.implicits.global
  import scala.concurrent.duration.DurationInt
  import cats.effect.std.Queue

  def handle(
      data: (Long, Int),
      counter: Ref[IO, Long],
      min: Ref[IO, Int],
      queue: Queue[IO, Option[(Long, Int)]]
  ): IO[Unit] =
    val (num, amount) = data
    val tracking = for {
      oldMin <- min.get
      _ <-
        if oldMin > amount then min.set(amount)
        else IO.unit
    } yield ()
    val eff =
      if amount == 0 then
        for {
          count <- counter.updateAndGet(total => total + 1)
          _     <- queue.offer(None)
          // _     <- IO.println(count)
        } yield ()
      else if (num == 0) {
        queue.offer(Some((1L, amount - 1)))
      } else if (num.toString.length() % 2 == 0) {
        val digits          = num.toString()
        val (first, second) = digits.splitAt(digits.length() / 2)
        queue.offer(Some((first.toLong, amount - 1))) *> queue.offer(
          Some((second.toLong, amount - 1))
        )
      } else {
        queue.offer(Some((num * 2024, amount - 1)))
      }
    tracking *> eff

  override def part2: Unit = {
    val amount    = 25
    val data      = Utils.readDailyResourceIntoString(11)
    val nums      = data.split(" ").map(_.toLong)
    val initItems = nums.map(n => (n, amount)).toSeq

    val program: IO[Long] = for {
      q       <- Queue.unbounded[IO, Option[(Long, Int)]]()
      counter <- Ref[IO].of(0L)
      min     <- Ref[IO].of(Int.MaxValue)
      streamFromQueue = Stream.fromQueueNoneTerminated(q)
      _ <- initItems.map(tuple => q.offer(Some(tuple))).sequence
      _ <- streamFromQueue
        .evalMap(handle(_, counter, min, q))
        .compile
        .drain
      count <- counter.get
    } yield count

    val count = program.unsafeRunSync()
    println(count)
  }

}

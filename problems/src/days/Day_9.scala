package days

import model.Day
import model.Utils
import scala.collection.mutable.ArrayBuffer
import fastparse.internal.Util

case object Day_9 extends Day {

  private val tinyExampleMap = "12345"

  private val exampleData = "2333133121414131402"

  private val exampleDataPlusAFew = "23331331214141314025555"

  case class Drive(ab: ArrayBuffer[Int]) {
    override def toString(): String =
      "|" + ab.map(i => if i == -1 then "." else i.toString()).mkString("|") + "|"

    def defrag() = {
      var lPtr = 0
      var rPtr = ab.length - 1
      while (lPtr < rPtr) {
        // Move lPtr onto an empty
        while (ab(lPtr) != -1) {
          lPtr += 1
        }
        val e = ab(lPtr)

        // Move rPtr onto a non-empty
        while (ab(rPtr) == -1) {
          rPtr -= 1
        }
        val n = ab(rPtr)

        // swap them if they're still ordered correctly
        if lPtr < rPtr then
          ab.update(lPtr, n)
          ab.update(rPtr, e)
        //   println(s"Pointer Distance: ${rPtr - lPtr}")
      }
    }

    def checksum() = {
      ab.zipWithIndex.map((id, index) => if id == -1 then 0 else id.toLong * index.toLong).sum
    }
  }

  object Parsing {
    def parse(s: String) =
      val a = ArrayBuffer[Int]()
      //   println(s"Parsing string of length: ${s.length()}")
      s.toCharArray
        .grouped(2)
        .zipWithIndex
        .foreach((arr, id) => {
          //   println(s"Arr: [${arr.mkString(", ")}] | ID: $id")
          val file = arr(0).toString().toInt
          //   println(s"Adding $id $file times...")
          (1 to file).foreach { _ =>
            a.addOne(id)
          }
          if (arr.length == 2) {
            var empty = arr(1).toString().toInt
            // println(s"Adding empty space $empty times...")
            (1 to empty).foreach { _ =>
              a.addOne(-1)
            }
          }
        })
      Drive(a)
  }

  override def example: Unit = {
    val drive = Parsing.parse(exampleDataPlusAFew)
    drive.defrag()
    println(drive.checksum())
  }

  override def part1: Unit = {
    val drive = Parsing.parse(Utils.readDailyResourceIntoString(9))
    drive.defrag()
    println(drive.checksum())
  }

}

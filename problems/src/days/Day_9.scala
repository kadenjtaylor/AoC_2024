package days

import model.Day
import model.Utils
import scala.collection.mutable.ArrayBuffer
import days.Day_9.Block.Empty

object Day_9 extends Day {

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

  trait Sized {
    def size: Int
  }

  enum Block extends Sized:
    case File(id: Int, size: Int)
    case Empty(size: Int)

    def isEmpty: Boolean = this match
      case File(_, _) => false
      case Empty(_)   => true

  case class FileDrive(ab: ArrayBuffer[Block]) {

    override def toString(): String =
      "|" + ab.map(b => if b.isEmpty then "." * b.size else "*" * b.size).mkString("|") + "|"

    def defrag() = {
      //   println(this.toDrive().toString())
      var rPtr = ab.length - 1
      while (rPtr > 0) {
        // Move rPtr onto a non-empty
        while (ab(rPtr).isEmpty) {
          rPtr -= 1
        }
        val n = ab(rPtr)
        // println(s"Need to find a place to put: $n")

        val firstEmptyOfCorrectSize =
          ab.zipWithIndex
            .collectFirst({
              case (Block.Empty(size), index) if index < rPtr && size >= n.size => (index, size)
            })

        // println(s"Found: $firstEmptyOfCorrectSize")

        firstEmptyOfCorrectSize match
          case Some((emptyIndex, emptySize)) => {
            // println(s"Setting index $emptyIndex to $n")
            ab.update(emptyIndex, n)
            // println(s"Setting index $rPtr to Empty(${n.size})")
            ab.update(rPtr, Block.Empty(n.size))
            // println(s"Inserting remaining extra space after file")
            ab.insert(emptyIndex + 1, Empty(emptySize - n.size))
            // println(s"Adding +1 to rPtr to counter longer list")
            rPtr += 1
          }
          case None => {
            // println(s"Skipping $n")
          }

        rPtr -= 1
        // println(this.toDrive().toString())

      }
    }
    def toDrive() = {
      Drive(
        ab.flatMap(block =>
          block match
            case Block.File(id, size) => List.fill(size)(id)
            case Block.Empty(size)    => List.fill(size)(-1)
        )
      )
    }
  }

  object Parsing {
    def parse(s: String): Drive =
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

    def parse2(s: String): FileDrive =
      val a = ArrayBuffer[Block]()
      s.toCharArray
        .grouped(2)
        .zipWithIndex
        .foreach((arr, id) => {
          val fileSize = arr(0).toString().toInt
          a.addOne(Block.File(id, fileSize))
          if (arr.length == 2) {
            var emptySize = arr(1).toString().toInt
            a.addOne(Block.Empty(emptySize))
          }
        })
      FileDrive(a)
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

  override def part2: Unit = {
    val fileDrive = Parsing.parse2(Utils.readDailyResourceIntoString(9))
    fileDrive.defrag()
    println(fileDrive.toDrive().checksum())
  }

}

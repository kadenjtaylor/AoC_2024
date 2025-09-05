package days

import model.Day
import model.Utils
import scala.collection.mutable.ArrayBuffer

case object Day_9 extends Day {

  private val tinyExampleMap = "12345"

  private val exampleData = "2333133121414131402"

  trait Sized {
    def size: Int
  }

  enum Block extends Sized:
    case Empty(size: Int)
    case File(id: Char, size: Int)

    def isFile = this match
      case Empty(_)   => false
      case File(_, _) => true

    def splitAt(n: Int): List[Block] = this match
      case Empty(size) if n > 0 && n < size => List(Empty(n), Empty(size - n))
      case File(id, size)                   => List(File(id, n), File(id, size - n))

  case class FileSystem(blocks: List[Block]) {

    def toCharList(): List[Char] =
      blocks
        .flatMap(b =>
          b match
            case Block.Empty(size)    => Array.fill(size)('.')
            case Block.File(id, size) => Array.fill(size)(id)
        )

    override def toString(): String =
      blocks
        .map(b =>
          b match
            case Block.Empty(size)    => "." * size
            case Block.File(id, size) => id.toString() * size
        )
        .mkString

    // def blockBorders(): String =
    //   "|" + blocks
    //     .map(b =>
    //       b match
    //         case Block.Empty(size)    => "." * size
    //         case Block.File(id, size) => "X" * size
    //     )
    //     .mkString("|") + "|"

    // def checksum() =
    //   this
    //     .toString()
    //     .toCharArray()
    //     .map(c => if c == '.' then '0' else c) // make '.' -> 0 for this calc
    //     .zipWithIndex
    //     .map((c, i) => c.toString().toLong * i)
    //     .sum

    def emptySpace() = blocks
      .map(_ match {
        case Block.Empty(size) => size
        case _                 => 0
      })
      .sum

    def files() = blocks
      .map(_ match {
        case Block.File(_, _) => 1
        case _                => 0
      })
      .sum

    // def split(blockIndex: Int, cutpoint: Int): FileSystem = {
    //   val (front, back) = blocks.splitAt(blockIndex)
    //   val maybeNewThing = back.headOption.map(b => b.splitAt(cutpoint))
    //   val newBlocks = maybeNewThing match
    //     case Some(value) => front ++ value ++ back.drop(1)
    //     case None        => front ++ back
    //   FileSystem(newBlocks)
    // }

    // private def firstEmptyIndex: Int =
    //   blocks.indexWhere(!_.isFile)

    // private def lastFileIndex: Int =
    //   blocks.lastIndexWhere(_.isFile)

    // def defrag(): FileSystem = {
    //   var myself = this
    //   println(s"$myself - ${myself.checksum()}")
    //   while (myself.firstEmptyIndex < myself.lastFileIndex) {
    //     myself = myself.oneStepDefrag()
    //     println(s"${myself.checksum()} / ${myself.emptySpace()}")
    //   }
    //   myself
    // }

    // def oneStepDefrag(): FileSystem = {
    //   val firstEmptyIndex = blocks.indexWhere(!_.isFile)
    //   val lastFileIndex   = blocks.lastIndexWhere(_.isFile)

    //   val firstEmpty = blocks(firstEmptyIndex)
    //   val lastFile   = blocks(lastFileIndex)

    //   firstEmpty.size - lastFile.size match
    //     case i if i < 0 => {
    //       val v1 = split(lastFileIndex, -i)
    //       val v2 = FileSystem(replace(v1.blocks)(firstEmptyIndex, v1.blocks(lastFileIndex + 1)))
    //       val v3 = FileSystem(replace(v2.blocks)(lastFileIndex + 1, v1.blocks(firstEmptyIndex)))
    //       v3
    //     }
    //     case i if i > 0 => {
    //       val v1 = split(firstEmptyIndex, lastFile.size)
    //       val v2 = FileSystem(replace(v1.blocks)(firstEmptyIndex, lastFile))
    //       val v3 = FileSystem(replace(v2.blocks)(lastFileIndex + 1, v1.blocks(firstEmptyIndex)))
    //       v3
    //     }
    //     case _ => {
    //       val first  = replace(blocks)(firstEmptyIndex, lastFile)
    //       val second = FileSystem(replace(first)(lastFileIndex, firstEmpty))
    //       second
    //     }
    // }
  }

  def fillPercentage(list: List[Char]) = {
    list
      .count(_ match {
        case '.' => false
        case _   => true
      })
      .toDouble / list.length
  }

  def fillGraph(chars: List[Char], width: Int = 100) =
    val amount = if chars.length > width then chars.length / width else 1
    chars
      .grouped(amount)
      .map(fillPercentage(_))
      .map(i => if i < 0.25 then '_' else if i < 0.5 then '-' else if i < 0.75 then '=' else '#')
      .mkString

  def checksum(charList: List[Char]) =
    charList
      .map(c => if c == '.' then '0' else c) // make '.' -> 0 for this calc
      .zipWithIndex
      .map((c, i) => c.toString().toLong * i.toLong)
      .sum

  def charListCompress(l: List[Char]): List[Char] = {
    val totalEmpties   = l.count(_ == '.')
    val (begin, end)   = l.splitAt(l.length - totalEmpties)
    val emptiesInBegin = begin.zipWithIndex.flatMap((c, i) => if c == '.' then Some(i) else None)
    val filesToMove    = end.reverse.filter(_ != '.')
    val moves          = emptiesInBegin.zip(filesToMove)
    // println(s"Empties: ${filesToMove.length}")
    // val newBegin = begin.zipWithIndex.map((c, i) =>
    //   moves.get(i) match
    //     case Some(newChar) => newChar
    //     case None          => c
    // )
    moves.foldLeft(begin)((current, move) => {
      println(fillGraph(current, 325))
      replace(current)(move._1, move._2)
    }) ++ List.fill(totalEmpties)('.')
  }

//   def swap[T](list: List[T])(a: Int, b: Int) = {
//     val aItem = list(a)
//     val bItem = list(b)
//     val v1    = replace(list)(a, bItem)
//     val v2    = replace(v1)(b, aItem)
//     v2
//   }

  object FileSystem2 {
    def apply(chars: List[Char]): FileSystem2 =
      FileSystem2(
        chars.zipWithIndex
          .flatMap((c, i) => if c == '.' then None else Some((i, c)))
          .toMap,
        chars.length
      )
  }
  case class FileSystem2(var files: Map[Int, Char], top: Int) {
    override def toString(): String =
      Range(0, top)
        .map(i =>
          files.get(i) match
            case Some(char) => char
            case None       => '.'
        )
        .mkString

    def checksum() =
      files
        .map((i, c) => c.toString().toLong * i)
        .sum

    def emptyIndicies() = // TODO: Top + 1?
      Range(0, top).flatMap(i => if files.get(i).isEmpty then Some(i) else None)

    def defrag(): Unit =
      var k    = files.keySet.max
      var gaps = emptyIndicies()
      while (gaps.length > 0 && gaps.head < k) {
        println(this)
        val v = files(k)
        files = files.removed(k)
        files = files.updated(gaps.head, v)
        k = files.keySet.max
        gaps = gaps.tail
      }

    def defraggedChecksum() = {
      var total: Long = 0
      println(s"Running from 0 to $top...")
      var nums = Range(0, top).toList
      var k    = files.keySet.max
      while (nums.head < k) {
        val index = nums.head
        val increment: Long = files.get(index) match
          case Some(value) => {
            // println(s"Got it, value is $value")
            value.toString().toInt
          }
          case None => {
            //   println(s"It's empty, need to pull a file from the back")
            val v = files(k)
            files = files.removed(k)
            v.toString().toInt
          }
        if index % 100 == 0 then println(s"$total += ($increment * $index)")
        total += (increment * index)
        nums = nums.tail
        k = files.keySet.max
      }
      total
    }
  }

  def replace[T](list: List[T])(index: Int, newItem: T): List[T] = {
    (list.splitAt(index)._1 :+ newItem) ++ list.splitAt(index)._2.drop(1)
  }

  object Parsing {

    def toChar(i: Int): Char = {
      i.toString().charAt(0)
    }

    def parse(s: String): FileSystem = {
      var blocks = List[Block]()
      var idNum  = 0
      s.toCharArray().grouped(2).foreach { chunk =>
        val newBlocks: List[Block] =
          if (chunk.length == 1) {
            List(Block.File(toChar(idNum), chunk(0).toString().toInt))
          } else {
            List(
              Block.File(toChar(idNum), chunk(0).toString().toInt),
              Block.Empty(chunk(1).toString().toInt)
            )
          }
        blocks = blocks.appendedAll(newBlocks)
        idNum += 1
      }
      FileSystem(blocks)
    }
  }

  override def example: Unit = {
    val data  = Parsing.parse(exampleData)
    val newFs = FileSystem2(data.toCharList())
    println(newFs)
    newFs.defrag()
    println(newFs)
    println(newFs.checksum())
    // val result = charListCompress(data.toCharList())
    // println(result)
    // println(checksum(result))
  }

  override def part1: Unit = {
    // val data  = Parsing.parse(Utils.readDailyResourceIntoString(9))
    // val newFs = FileSystem2(data.toCharList())
    // println(newFs)
    // newFs.defrag()
    // println(newFs)
    // println(newFs.checksum())
    val data  = Parsing.parse(Utils.readDailyResourceIntoString(9))
    val newFs = FileSystem2(data.toCharList())
    println(newFs.defraggedChecksum())
    ()
  }

  object Tests {
    def charReverse() = {
      val data = List.fill(6)('.') ++ Range(0, 10).map(_.toString().toCharArray()(0)).toList
      val fs   = FileSystem2(data)
      println(fs)
      fs.defrag()
      println(fs)
    }

    /*
        This shows that, from the numbers:
            - the final value for any input of this length cannot be higher than 40.78B
              (and that's if the WHOLE char array was 9s)
            - Given that we have SOME empty cells (45017 of them), that max is now 11.33B
              (that's in a scenario where all numbers (still all 9s) are packed to the left)
            - according to the current code, the STARTING checksum is 14809517540 (which is not
              only a valid phone number, but is SO much lower than the other 2 numbers)
     */
    def numTest() = {
      val data  = Parsing.parse(Utils.readDailyResourceIntoString(9))
      val newFs = FileSystem2(data.toCharList())

      var total: Long     = 0
      val numEmpties: Int = newFs.emptyIndicies().length
      val numBlocks: Int  = newFs.files.keySet.size
      println(s"Blocks: $numBlocks, Empties: $numEmpties, Total: ${numBlocks + numEmpties}")
      val currentChecksum = newFs.checksum()
      println(s"Starting Checksum: $currentChecksum")
      Range(0, numBlocks).foreach { i =>
        total += (i * 9)
        if i % 1000 == 0 then println(s"$i: $total")
      }
      println(s"  Final: $total")
    }
  }
}

package days

import model.Day
import model.Utils

object Day_7 extends Day {

  private def exampleData = """190: 10 19
                              |3267: 81 40 27
                              |83: 17 5
                              |156: 15 6
                              |7290: 6 8 6 15
                              |161011: 16 10 13
                              |192: 17 8 14
                              |21037: 9 7 18 13
                              |292: 11 6 16 20""".stripMargin

  case class Equation(result: Long, nums: Array[Long]) {

    /* Generate very possible List[Operator] of length (nums - 1)
    that only contains members of ops. Return the true ones. */
    def solveWith(ops: Set[Operator]): List[List[Operator]] = {
      generate(nums.length - 1, ops).filter(attempt(_))
    }

    private def generate(length: Int, options: Set[Operator]): List[List[Operator]] = {
      var myList  = List[List[Operator]](List())
      var counter = length
      while (counter > 0) {
        myList = addOperatorOptions(myList, options)
        counter -= 1
      }
      myList
    }

    private def addOperatorOptions(
        l: List[List[Operator]],
        ops: Set[Operator]
    ): List[List[Operator]] = {
      l.flatMap(ls => ops.map(o => ls :+ o))
    }

    private def attempt(ops: List[Operator]): Boolean = {
      assert(
        ops.length == nums.length - 1,
        s"Wrong number of operators (${ops.size}) given for '${this}'"
      )
      val first = nums(0)
      val rest  = ops.zip(nums.drop(1))
      val answer = rest.foldLeft(first)((runningTotal, operation) =>
        operation._1 match
          case Operator.Addition       => runningTotal + operation._2
          case Operator.Multiplication => runningTotal * operation._2
          case Operator.Concatenation  => (runningTotal.toString() + operation._2.toString()).toLong
      )
      result == answer
    }

    override def toString(): String = s"${result} : ${nums.mkString(" ? ")}"
  }

  enum Operator:
    case Addition, Multiplication, Concatenation

    override def toString(): String = this match
      case Addition       => "+"
      case Multiplication => "*"
      case Concatenation  => "||"

  object Parsing {
    import fastparse._, NoWhitespace._

    def number[$: P] = P(CharIn("0-9").rep(1).!.map(digits => digits.toLong))

    def nums[$: P] = P(number.rep(sep = " "))

    def equation[$: P] =
      P(number ~ ": " ~ nums).map((i, nums) => Equation(i, nums.toArray))

    def wholeThing[$: P] = P(equation.rep(sep = "\n") ~ End)

    def parseData(s: String): List[Equation] =
      fastparse.parse(s, Parsing.wholeThing).get.value.toList
  }

  def solve(eqs: List[Equation], ops: Set[Operator]): List[Long] = {
    eqs.flatMap(e =>
      val solutions = e.solveWith(ops)
      if solutions.isEmpty then None
      else Some(e.result)
    )
  }

  override def example: Unit = {
    val eqs     = Parsing.parseData(exampleData)
    val results = solve(eqs, Set(Operator.Addition, Operator.Multiplication))
    println(s"Results Found: ${results.length} / Total: ${results.sum}")
  }

  override def part1: Unit = {
    val eqs     = Parsing.parseData(Utils.readDailyResourceIntoString(7))
    val results = solve(eqs, Set(Operator.Addition, Operator.Multiplication))
    println(s"Results Found: ${results.length} / Total: ${results.sum}")
  }

  override def part2: Unit = {
    val eqs = Parsing.parseData(Utils.readDailyResourceIntoString(7))
    val results =
      solve(eqs, Set(Operator.Addition, Operator.Multiplication, Operator.Concatenation))
    println(s"Results Found: ${results.length} / Total: ${results.sum}")
  }
}

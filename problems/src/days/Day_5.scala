package days

import model.Day
import days.Day_5.Parsing.parseData
import days.Day_5.DataManipulation.validate
import days.Day_5.DataManipulation.middleElement
import model.Utils
import days.Day_5.DataManipulation.fixUpdate

case object Day_5 extends Day {

  private def exampleData = """47|53
                              |97|13
                              |97|61
                              |97|47
                              |75|29
                              |61|13
                              |75|53
                              |29|13
                              |97|29
                              |53|29
                              |61|53
                              |97|53
                              |61|29
                              |47|13
                              |75|47
                              |97|75
                              |47|61
                              |75|61
                              |47|29
                              |75|13
                              |53|13
                              |
                              |75,47,61,53,29
                              |97,61,53,29,13
                              |75,29,13
                              |75,97,47,61,53
                              |61,13,29
                              |97,13,75,29,47""".stripMargin

  // ====================================================== //

  type Rule    = (Int, Int)
  type Update  = List[Int]
  type Data    = (List[Rule], List[Update])
  type Follows = Map[Int, Int]

  object Parsing {
    import fastparse._, SingleLineWhitespace._

    def number[$: P] = P(CharIn("0-9").rep.!.map(_.toInt))

    def rule[$: P] = P(number ~ "|" ~ number)

    def update[$: P] = P(number.rep(sep = ","))

    def ruleSection[$: P] = P(rule.rep(sep = "\n"))

    def updateSection[$: P] = P(update.rep(sep = "\n"))

    def parseData(s: String): Data =
      val parts         = s.split("\n\n")
      val rulesResult   = fastparse.parse(parts(0), Parsing.ruleSection)
      val updatesResult = fastparse.parse(parts(1), Parsing.updateSection)
      (rulesResult.get.value.toList, updatesResult.get.value.map(_.toList).toList)
  }

  object DataManipulation {

    def validate(rules: List[Rule], update: Update) =
      val truths = for {
        i <- Range(0, update.length)
        j <- Range(i + 1, update.length)
      } yield (update(i), update(j))
      rules
        .filter((a, b) => update.contains(a) && update.contains(b))
        .map(rule => truths.contains(rule))
        .reduce(_ && _)

    def middleElement[T](l: List[T]): Option[T] =
      l.length % 2 match
        case 0 => None
        case 1 => Some(l(l.length / 2))

    def fixUpdate(rules: List[Rule], update: Update): Update =
      val rulesINeed = rules
        .filter((a, b) => update.contains(a) && update.contains(b))
      update.sortWith((a, b) => rulesINeed.contains((a, b)))
  }

  // ====================================================== //

  override def example: Unit =
    val (rules, updates) = parseData(exampleData)
    val result = updates
      .filter(u => validate(rules, u))
      .flatMap(u => middleElement(u))
      .sum
    println(s"Sum of middle pages: $result")

  override def part1: Unit =
    val (rules, updates) = parseData(Utils.readDailyResourceIntoString(5))
    val result = updates
      .filter(u => validate(rules, u))
      .flatMap(u => middleElement(u))
      .sum
    println(s"Sum of middle pages: $result")

  override def part2: Unit =
    val (rules, updates) = parseData(Utils.readDailyResourceIntoString(5))
    val result = updates
      .filter(u => !validate(rules, u))
      .map(u => fixUpdate(rules, u))
      .flatMap(u => middleElement(u))
      .sum
    println(s"Sum of fixed middle pages: $result")

}

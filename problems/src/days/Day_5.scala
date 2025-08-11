package days

import model.Day

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

  type Follows = Map[Int, Int]

  object Parsing {
    import fastparse._, SingleLineWhitespace._

    def number[$: P] = P(CharIn("0-9").rep.!.map(_.toInt))

    def rule[$: P] = P(number ~ "|" ~ number)

    def update[$: P] = P(number.rep(sep = ","))

    def ruleSection[$: P] = P(rule.rep(sep = "\n"))

    def updateSection[$: P] = P(update.rep(sep = "\n"))
  }

  // ====================================================== //

//   val number = P(CharIn('0' to '9').rep(1).!.map(_.toInt))

  override def example: Unit =
    // Okay, I still have yet to create a parser that covers the whole thing
    // But I can make it work w/ sections now... which is good enough to move on
    val parts         = exampleData.split("\n\n")
    val rulesResult   = fastparse.parse(parts(0), Parsing.ruleSection)
    val updatesResult = fastparse.parse(parts(1), Parsing.updateSection)
    println(rulesResult.get.value)
    println(updatesResult.get.value)

  override def part1: Unit = ()

  override def part2: Unit = ()

}

package days

import model.Day

case object Day_12 extends Day {

  private val tinyExample = """AAAA
                              |BBCD
                              |BBCC
                              |EEEC""".stripMargin

  private val nestedExample = """OOOOO
                                |OXOXO
                                |OOOOO
                                |OXOXO
                                |OOOOO""".stripMargin

  private val biggerExample = """RRRRIICCFF
                                |RRRRIICCCF
                                |VVRRRCCFFF
                                |VVRCCCJFFF
                                |VVVVCJJCFE
                                |VVIVCCJJEE
                                |VVIIICJJEE
                                |MIIIIIJJEE
                                |MIIISIJEEE
                                |MMMISSJEEE""".stripMargin

  override def example: Unit = {
    ()
  }
}

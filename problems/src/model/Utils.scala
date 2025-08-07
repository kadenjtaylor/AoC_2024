package model

import os.*

object Utils {
  // TODO: Stuff for reading files in
  def readDailyResourceIntoString(i: Int): String =
    val p = os.pwd / "problems" / "resources" / s"day_$i.txt"
    os.read(p)
}

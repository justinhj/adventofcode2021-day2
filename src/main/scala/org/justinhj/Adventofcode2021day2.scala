package org.justinhj

import scala.io.Source

object AOCUtil {
  def inputToStrings(name: String): List[String] = {
    Source.fromResource(name).getLines().toList
  }
}


object Adventofcode2021day2 extends App {

  import AOCUtil._

  val sampleInput = """forward 5
    down 5
    forward 8
    up 3
    down 8
    forward 2
  """.strip

  val input = inputToStrings("day2.txt")

  println(s"input has ${input.size}")

}

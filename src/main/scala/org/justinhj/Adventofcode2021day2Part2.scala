package org.justinhj

object Adventofcode2021day2Part2 extends App {
  import AOCUtil._

  case class Position(horizontal: Int, depth: Int, aim: Int)

  def parseAndApply(input: String, position: Position): Position = {
    // IRL don't ignore errors when parsing and converting...
    val pattern = """([a-z]+) (\d+)""".r
    val pattern(command,xi) = input
    val x = xi.toInt    
    
    command match {
      case "forward" =>
        position.copy(horizontal = position.horizontal + x,
          depth = position.depth + position.aim * x)
      case "up" =>
        position.copy(aim = position.aim - x)
      case "down" =>
        position.copy(aim = position.aim + x)
    }
  }

  val sampleInput = """forward 5
      |down 5
      |forward 8
      |up 3
      |down 8
      |forward 2""".stripMargin.split("\n").toList

  val part1Input = inputToStrings("day2.txt")
 
  def solve(input: List[String]): Int = {
    val finalPos = input.foldLeft(Position(0,0,0)) {
      case (position, input) =>
        parseAndApply(input, position)
      }
    finalPos.depth * finalPos.horizontal
  }

  println(s"sample ${solve(sampleInput)}")
  println(s"part1 ${solve(part1Input)}")
}

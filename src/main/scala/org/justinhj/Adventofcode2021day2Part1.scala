package org.justinhj

import scala.io.Source
import scala.util.Try
import zio.prelude._

object AOCUtil {
  def inputToStrings(name: String): List[String] = {
    Source.fromResource(name).getLines().toList
  }
}

object Adventofcode2021day2 extends App {

  import AOCUtil._

  case class Vec2(x: Int, y: Int) {
    def scale(n: Int): Vec2 = {
      Vec2(x * n, y * n)
    }
    def prod(): Int = {
      x * y
    }
  }

  implicit val assoc = new Associative[Vec2] {
    def combine(l: => Vec2, r: => Vec2): Vec2 = {
      Vec2(l.x + r.x, l.y + r.y)
    }
  }

  val moveMap = Map(
    "up" -> Vec2(0,-1),
    "down" -> Vec2(0,1),
    "forward" -> Vec2(1,0)
  )

  def parse(input: List[String]): List[Vec2] = {
      input.map {
        line =>
          val pattern = """([a-z]+) (\d+)""".r

          val parsed = Try {
            val pattern(command,scale) = line            
            (command,scale.toInt)
          }.toOption

          
          (for (
            (c,s) <- parsed;
            v <- moveMap.get(c)
          ) yield v.scale(s)).get
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
    val nelMoves = NonEmptyList.fromIterableOption(parse(input))
    val finalPos = nelMoves.get.reduce
    finalPos.prod()
  }

  println(s"sample ${solve(sampleInput)}")
  println(s"part1 ${solve(part1Input)}")
}

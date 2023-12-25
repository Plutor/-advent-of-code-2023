import scala.io.Source

def getData: List[String] =
  Source.fromFile("testdata").getLines.toList
  //Source.fromFile("data").getLines.toList

def part1: Long =
  1

def part2: Long =
  2

@main def hello: Unit =
  println("Part 1: " + part1)
  println("Part 2: " + part2)


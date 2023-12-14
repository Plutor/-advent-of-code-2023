import scala.io.Source
import scala.collection.mutable._

def getData: List[String] =
  Source.fromFile("data").getLines.toList

def steps(data: List[String]): List[Char] =
  data(0).toList

def rules(data: List[String]): Map[String, Map[Char, String]] =
  var rules = Map[String, Map[Char, String]]()
  for (n <- 2 to data.size-1) {
    rules += (data(n).substring(0, 3) -> Map[Char, String]('L' -> data(n).substring(7, 10),
                                                           'R' -> data(n).substring(12, 15)))
  }
  rules

def part1(data: List[String]): Long =
  val s = steps(data)
  val r = rules(data)
  var current = "AAA"
  var stepnum: Long = 0
  while (current != "ZZZ") {
    val step = s((stepnum % s.size).toInt)
    stepnum += 1
    current = r(current)(step)
  }
  stepnum

def part2(data: List[String]): Long =
  val s = steps(data)
  val r = rules(data)
  var currents = r.keys.filter(_.toList.last=='A')
  var stepnum: Long = 0
  while (currents.filter(_.toList.last!='Z').size > 0) {
    // println(currents)
    val step = s((stepnum % s.size).toInt)
    stepnum += 1
    currents = currents.map(r(_)(step))
  }
  stepnum

@main def hello: Unit =
  println("Part 1: " + part1(getData))
  println("Part 2: " + part2(getData))


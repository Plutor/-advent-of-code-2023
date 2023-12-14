import scala.io.Source
import scala.collection.mutable.ListBuffer

def getData: List[String] =
  Source.fromFile("data").getLines.toList

def getTotal(getNums: String => List[Int]): Int =
  var total = 0
  for (line <- getData) {
    val r = getNums(line)
    val v = r(0) * 10 + r.last
    total += v
  }
  total

def part1(s: String): List[Int] =
  var r = new ListBuffer[Int]()
  for (ch <- s.toList) {
    if (ch >= '0' && ch <= '9') {
      r.append(ch.toInt - 48)
    }
  }
  r.toList

def part2(s: String): List[Int] =
  var r = new ListBuffer[Int]()
  for (v <- 0 to s.length-1) {
    if (s(v) >= '0' && s(v) <= '9') {
      r.append(s(v).toInt - 48)
    }
    if (s.substring(v).startsWith("one"))    { r += 1 }
    if (s.substring(v).startsWith("two"))    { r += 2 }
    if (s.substring(v).startsWith("three"))  { r += 3 }
    if (s.substring(v).startsWith("four"))   { r += 4 }
    if (s.substring(v).startsWith("five"))   { r += 5 }
    if (s.substring(v).startsWith("six"))    { r += 6 }
    if (s.substring(v).startsWith("seven"))  { r += 7 }
    if (s.substring(v).startsWith("eight"))  { r += 8 }
    if (s.substring(v).startsWith("nine"))   { r += 9 }
  }
  r.toList

@main def hello: Unit =
  println("Part 1: " + getTotal(part1))
  println("Part 2: " + getTotal(part2))


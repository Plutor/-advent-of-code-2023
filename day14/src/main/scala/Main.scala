import scala.io.Source
import scala.collection.mutable._
import scala.util.matching.Regex

def getData: List[String] =
  //Source.fromFile("testdata").getLines.toList
  Source.fromFile("data").getLines.toList

def rotateClockwise(f: List[String]): List[String] =
  (0 to f(0).size-1).map(x=>f.map(r=>r(x)).reverse.mkString).toList

def rollColumnEast(c: String): String =
  var cc = c
  val rollAreaPattern: Regex = "[O\\.]+".r
  for m <- rollAreaPattern.findAllMatchIn(c) do
    cc = cc.substring(0, m.start) + m.matched.toList.sorted.mkString + c.substring(m.end)
  cc

def scoreColumn(c: String): Int =
  c.zipWithIndex.map((v,i) =>
      if (v == 'O') i+1 else 0).sum

def part1: Long =
  val f = getData
  rotateClockwise(f)
    .map(c=>rollColumnEast(c))
    .map(c=>scoreColumn(c))
    .sum

def part2: Long =
  var f = getData
  var loads = ListBuffer[Int]()
  loads += f.map(c=>scoreColumn(c)).sum
  val trysize = 1234
  for (i <- 1 to trysize) {
    for (j <- 0 to 3) {
      f = rotateClockwise(f).map(c=>rollColumnEast(c))
    }
    loads += rotateClockwise(f).map(c=>scoreColumn(c)).sum
  }
  // Find values that equal the last value
  // This is incredibly janky, there are no heroes here
  val cycles = loads.zipWithIndex.filter((v,i)=>v == loads.last)
  val cyclestart = cycles.dropRight(1).last(1)
  loads( cyclestart + ((1000000000L - trysize) % (trysize - cyclestart)).toInt )

@main def hello: Unit =
  println("Part 1: " + part1)
  println("Part 2: " + part2)


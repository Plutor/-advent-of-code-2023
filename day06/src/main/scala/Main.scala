import scala.io.Source

def getData: List[String] =
  Source.fromFile("data").getLines.toList

def getRaces: List[(Long, Long)] =
  val d = getData
  d(0).split(" ").filter(_!="").drop(1).map(_.toLong).zip(d(1).split(" ").filter(_!="").drop(1).map(_.toLong)).toList

def getRaceRecordOptions(t: Long, r: Long): Long =
  (t-1 to 1L by -1).find(n => n*(t-n) > r).getOrElse(0L) - (1L to t-1).find(n => n*(t-n) > r).getOrElse(0L) + 1

def part1: Long =
  getRaces.map((t,r) => getRaceRecordOptions(t,r)).fold(1L)((a,b) => a*b)

def getOneRace: (Long, Long) =
  val d = getData
  (d(0).split(":")(1).replaceAll(" ", "").toLong, d(1).split(":")(1).replaceAll(" ", "").toLong)

def part2: Long =
  var tr = getOneRace
  getRaceRecordOptions(tr(0), tr(1))

@main def hello: Unit =
  println("Part 1: " + part1)
  println("Part 2: " + part2)


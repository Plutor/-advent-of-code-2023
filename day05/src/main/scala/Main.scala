import scala.io.Source
import scala.collection.mutable.Set
import scala.collection.immutable.Map

def getData: List[String] =
  Source.fromFile("data").getLines.toList

def getSeeds: List[String] =
  val w = getData(0).split(" ")
  w.slice(1, w.size).map(x=>s"seed $x").toList

class Mapping(srcType: String, srcStart: Long, srcEnd: Long,
              dstType: String, delta: Long):
  override def toString = s"{$srcType, $srcStart-$srcEnd => $dstType, $delta}"
  def follow(s: String): String =
    val typenum = s.split(" ")
    val num = typenum(1).toLong
    if (srcType == typenum(0) && srcStart <= num && srcEnd >= num)
      s"$dstType ${num+delta}"
    else ""
end Mapping

def getMap: Set[Mapping] =
  var m = Set[Mapping]()
  val data = getData
  val headPattern = """([^-]*)-to-([^-]*) map:""".r
  var dest, src: String = ""
  for (n <- 2 to data.size-1) {
    val line = data(n)
    line match {
      case headPattern(s, d) => src = s; dest = d
      case "" =>
      case _ =>
        val destsrclen = line.split(" ").map(_.toLong)
        m += Mapping(src, destsrclen(1), destsrclen(1)+destsrclen(2)-1,
                     dest, destsrclen(0)-destsrclen(1))
    }
  }
  m

def follow(src: String, m: Set[Mapping]): String =
  val defaultMapping = Map("seed" -> "soil", "soil" -> "fertilizer",
                           "fertilizer" -> "water", "water" -> "light",
                           "light" -> "temperature", "temperature" -> "humidity",
                           "humidity" -> "location")
  val f = m.map(r => r.follow(src)).filter(_ != "").toList
  if (f.size > 0) f(0)
  else {
    val typenum = src.split(" ")
    s"${defaultMapping(typenum(0))} ${typenum(1)}"
  }

def getLocations(s: List[String], m: Set[Mapping]): List[String] =
  var d = s
  for (_ <- 0 to 6) {
    d = d.map(follow(_, m))
    println(d)
  }
  d

def part1: Long =
  val l = getLocations(getSeeds, getMap)
  l.map(_.split(" ")(1).toLong).sorted.apply(0)

def getSeedRanges: List[Long] =
  val w = getData(0).split(" ")
  w.slice(1, w.size).map(_.toLong).toList

def part2: Long =
  val m = getMap
  var seeds = getSeedRanges
  var lowest: Long = 9999999999999
  var done: Long = 0
  var starttime = System.currentTimeMillis
  for (n <- 0 to seeds.length-1 by 2) {
    val start = seeds(n)
    val end = start + seeds(n+1) - 1
    for (s <- start to end) {
      var d = s"seed $s"
      for (_ <- 0 to 6) {
        d = follow(d, m)
      }
      val nd = d.split(" ")(1).toLong
      if (nd < lowest) {
        lowest = nd
        println(s"Best location: $lowest")
      }
      done += 1
       if (System.currentTimeMillis - starttime > 1000) {
        println(s"Done: $done")
        starttime = System.currentTimeMillis
      }
    }
  }
  lowest

@main def hello: Unit =
  println("Part 1: " + part1)
  println("Part 2: " + part2)


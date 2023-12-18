import scala.io.Source
import scala.collection.mutable._
import scala.math.pow

def getData: List[String] =
  Source.fromFile("data").getLines.toList
  //Source.fromFile("data").getLines.toList

def getRows: List[(String, Array[Int])] =
  getData.map(r =>
    val parts = r.split(" ")
    (parts(0), parts(1).split(",").map(_.toInt))
  ).toList

def getPossibilities(springs: String, counts: Array[Int]): List[String] =
  val missing = counts.sum - springs.filter(_=='#').size
  val unks = springs.filter(_=='?').size
  (0 to pow(2, unks).toInt-1)
    .map(_.toBinaryString).map(b => ("0" * (unks-b.size)) + b)
    .filter(_.filter(_=='1').size == missing)
    .map(b =>
        b.foldLeft(springs)((s, d) => s.replaceFirst("\\?", if (d=='1') "#" else "."))).toList

def matchesCounts(springs: String, counts: Array[Int]): Boolean =
  springs.split("\\.").filter(_!="").map(_.size).sameElements(counts)

def part1: Long =
  getRows.map((r,c) =>
      getPossibilities(r,c).filter(p=>matchesCounts(p,c)).size).sum.toLong

def part2: Long =
  2

@main def hello: Unit =
  println("Part 1: " + part1)
  println("Part 2: " + part2)


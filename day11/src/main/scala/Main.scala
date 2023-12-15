import scala.io.Source
import scala.collection.mutable._

def getData: List[String] =
  Source.fromFile("data").getLines.toList

def getGalaxies: Set[(Long, Long)] =
  val d = getData
  var g = Set[(Long,Long)]()
  for (y <- 0 to d.size-1) {
    for (x <- 0 to d(y).size-1) {
      if (d(y)(x) == '#') {
        g += (x,y)
      }
    }
  }
  g

def expandSpaceBy(before: Set[(Long,Long)], age: Long): Set[(Long,Long)] =
  val d = getData
  val rows = d.map(!_.contains('#'))
  val cols = (0 to d(0).size-1).map(n => !d.find(_(n) == '#').isDefined)
  before.map((bx,by) => (bx + (cols.take(bx.toInt).count(b=>b)*(age-1)), by + (rows.take(by.toInt).count(b=>b)*(age-1))))

def part1: Long =
  val g = expandSpaceBy(getGalaxies, 2).toList
  g.zipWithIndex.map((a, idx) =>
      g.take(idx).map(b =>
          (a(0)-b(0)).abs + (a(1)-b(1)).abs).sum).sum.toLong

def part2: Long =
  val g = expandSpaceBy(getGalaxies, 1000000L).toList
  g.zipWithIndex.map((a, idx) =>
      g.take(idx).map(b =>
          (a(0)-b(0)).abs + (a(1)-b(1)).abs).sum).sum.toLong

@main def hello: Unit =
  println("Part 1: " + part1)
  println("Part 2: " + part2)


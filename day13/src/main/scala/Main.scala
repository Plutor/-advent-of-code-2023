import scala.io.Source
import scala.collection.mutable._

def getData: List[String] =
  Source.fromFile("testdata").getLines.toList
  //Source.fromFile("data").getLines.toList

def getPatterns: List[List[String]] =
  var d = getData
  var patterns = ListBuffer[List[String]]()
  while (!d.isEmpty) {
    val pd = d.span(_ != "")
    println(pd(0))
    patterns += pd(0)
    if (!pd(1).isEmpty) d = pd(1).tail else d = pd(1)
  }
  patterns.toList

def findHorzReflection(p: List[String]): Int =
  p.init.zip(p.tail).zipWithIndex.filter((ab,i) => ab(0).sameElements(ab(1))).map(p=>p(1)).find(cr =>
      // println( (1 to p.size-1).filter(inc => cr+inc+1 <= p.size-1 && cr-inc >= 0) )
      // println( (1 to p.size-1).filter(inc => cr+inc+1 <= p.size-1 && cr-inc >= 0).find(inc => p(cr-inc) != p(cr+inc+1)) )
      !((1 to p.size-1).filter(inc => cr+inc+1 <= p.size-1 && cr-inc >= 0).find(inc => p(cr-inc) != p(cr+inc+1)).isDefined)
      ).getOrElse(-1) + 1

def findVertReflection(p: List[String]): Int =
  findHorzReflection((0 to p(0).size-1).map(n => p.map(_(n)).toString).toList)

def part1: Long =
  val p = getPatterns
  //println(p.map(findHorzReflection(_)))
  (p.map(findHorzReflection(_)).sum * 100 + p.map(findVertReflection(_)).sum).toLong

def part2: Long =
  2

@main def hello: Unit =
  println("Part 1: " + part1)
  println("Part 2: " + part2)


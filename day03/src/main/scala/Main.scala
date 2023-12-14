import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.util.{Try, Success, Failure}

def getData: List[String] =
  Source.fromFile("data").getLines.toList

class Partnum(var value: Int = 0, var y: Int = 0, var x1: Int = 0, var x2: Int = 0):
  override def toString = s"$value ($x1,$y -> $x2,$y)"
end Partnum

def partnums(s: List[String]): List[Partnum] =
  var parts = new ListBuffer[Partnum]()
  val p: Regex = """(\d+|\D+)""".r
  for (y <- 0 to s.length-1) {
    var x = 0
    for (m <- p.findAllMatchIn(s(y))) {
      var l = m.group(1).length
      Try(m.group(1).toInt) match {
        case Success(v) => parts += Partnum(v, y, x, x+l-1)
        case Failure(v) => 
      }
      x += l
    }
  }
  parts.toList

def isNearSymbol(p: Partnum, s: List[String]): Boolean =
  var ok = false
  for (y <- p.y-1 to p.y+1) {
    for (x <- p.x1-1 to p.x2+1) {
      if (y >=0 && y < s.length && x >=0 && x < s(y).length && // is in the boundaries
          s(y)(x) != '.' && (s(y)(x) < '0' || s(y)(x) > '9')) {   // is a symbol
            ok = true
      }
    }
  }
  ok

def part1(s: List[String]): Int =
  partnums(s).filter(p => isNearSymbol(p, s))
             .map(_.value)
             .fold(0)((x,y) => x+y)

def getGearRatio(parts: List[Partnum], x: Int, y: Int): Int =
  var gp = new ListBuffer[Int]()
  for (n <- 0 to parts.length-1) {
    if (parts(n).y >= y-1 && parts(n).y <= y+1 && parts(n).x1 <= x+1 && parts(n).x2 >= x-1)
      gp += parts(n).value
  }
  if (gp.length == 2) gp(0)*gp(1) else 0

def part2(s: List[String]): Int =
  val parts = partnums(s)
  var t = 0
  for (y <- 0 to s.length-1) {
    for (x <- 0 to s(y).length-1) {
      if (s(y)(x) == '*') {
        t += getGearRatio(parts, x, y)
      }
    }
  }
  t
  

@main def hello: Unit =
  val data = getData
  println("Part 1: " + part1(data))
  println("Part 2: " + part2(data))


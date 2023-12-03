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
  for (y <- p.y-1 to p.y+1) {
    for (x <- p.x1-1 to p.x2+1) {
      if (y >=0 && y < s.length && x >=0 && x < s(y).length && // is in the boundaries
          s(y)(x) != '.' && (s(y)(x) < '0' || s(y)(x) > '9')) {   // is a symbol
            return true
      }
    }
  }
  false

@main def hello: Unit =
  val data = getData
  println("Part 1: " + partnums(data).filter(p => isNearSymbol(p, data))
                                     .map(_.value)
                                     .fold(0)((x,y) => x+y))


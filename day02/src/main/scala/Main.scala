import scala.io.Source
import scala.util.matching.Regex

def getData: List[String] =
  Source.fromFile("data").getLines.toList

def isPossible(s: String): Boolean =
  val gamePattern: Regex = """(\d+) (\w+)""".r
  var ok = true
  for (m <- gamePattern.findAllMatchIn(s)) {
    val n = m.group(1).toInt
    m.group(2) match {
      case "red"   => if (n > 12) ok = false
      case "green" => if (n > 13) ok = false
      case "blue"  => if (n > 14) ok = false
    }
  }
  ok

def idIfPossible(s: String): Int =
  val rowPattern: Regex = """Game (\d+): (.*)""".r
  s match {
    case rowPattern(gameID, gamestr) => if (isPossible(gamestr)) gameID.toInt else 0
    case _ => 0
  }

def power(s: String): Int =
  val gamePattern: Regex = """(\d+) (\w+)""".r
  var r, g, b = 0
  for (m <- gamePattern.findAllMatchIn(s)) {
    val n = m.group(1).toInt
    m.group(2) match {
      case "red"   => if (n > r) r = n
      case "green" => if (n > g) g = n
      case "blue"  => if (n > b) b = n
    }
  }
  r * g * b

@main def hello: Unit =
  val data = getData
  val totalIDs = data.map(idIfPossible).fold(0)((x,y) => x+y)
  println("Part 1: " + totalIDs)
  val totalPowers = data.map(power).fold(0)((x,y) => x+y)
  println("Part 2: " + totalPowers)


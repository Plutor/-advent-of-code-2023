import scala.io.Source
import scala.math.pow
import scala.collection.mutable.Queue

class Card(val num: Int, win: Set[Int], have: Set[Int]):
  override def toString = s"$win | $have"
  def matches: Int =
    (win & have).size
  def points: Int =
    val m = matches
    if (m == 0) 0 else pow(2, m-1).toInt
end Card

def getData: List[String] =
  Source.fromFile("data").getLines.toList

def getCards: List[Card] =
  getData.map(toCard)

def toCard(l: String): Card =
  val num = l.split(":")(0).split(" ").last.toInt
  val parts = l.split(":")(1).split(" \\| ")
  Card(num, parts(0).split(" ").filter(_!="").map(_.toInt).toSet, parts(1).split(" ").filter(_!="").map(_.toInt).toSet)

def part1: Int =
  getCards.map(_.points).fold(0)((x,y)=>x+y)

def part2: Int =
  val cards = getCards
  var q = Queue[Card](getCards : _*)
  var count = 0
  while (!q.isEmpty) {
    val c = q.dequeue
    count += 1
    for (n <- c.num+1 to c.num+c.matches) {
      q += cards(n-1)
    }
  }
  count

@main def hello: Unit =
  println("Part 1: " + part1)
  println("Part 2: " + part2)
  


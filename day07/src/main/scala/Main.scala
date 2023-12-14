import scala.io.Source
import scala.collection.mutable._

def getData: List[String] =
  Source.fromFile("data").getLines.toList

class Hand(var cards: String, var bid: Long, jokers: Boolean) extends Ordered[Hand] {
  override def toString = s"$cards $bid"
  override def compare(that: Hand): Int = 
    val s = strength compareTo that.strength
    if (s == 0) cards compareTo that.cards else s
  def strength: Int =
    if (!jokers) _str(cards)
    else {
      var best = 0
      for (n <- 0 to cards.size-1) {
        val s = _str(cards.replaceAll("1", cards(n).toString))
        if (s > best) best = s
      }
      best
    }
  def _str(s: String): Int =
    var counts = Map[Char, Int]().withDefaultValue(0)
    s.toList.foreach(counts(_) += 1)
    if (counts.valuesIterator.contains(5)) 7
    else if (counts.valuesIterator.contains(4)) 6
    else if (counts.valuesIterator.contains(3) && counts.valuesIterator.contains(2)) 5
    else if (counts.valuesIterator.contains(3)) 4
    else if (counts.valuesIterator.filter(_==2).size == 2) 3
    else if (counts.valuesIterator.contains(2)) 2
    else 1
}

def getHands(jokers: Boolean = false): List[Hand] =
  getData.map(l =>
      val cb = l.split(" ")
      var h = cb(0).replaceAll("A", "E") .replaceAll("K", "D") .replaceAll("Q", "C") .replaceAll("T", "A")
      if (jokers) Hand(h.replaceAll("J", "1"), cb(1).toLong, jokers)
      else        Hand(h.replaceAll("J", "B"), cb(1).toLong, jokers)
  )

def scoreHands(h: List[Hand]): Long =
  var t: Long = 0
  for (n <- 0 to h.size-1)
    t += h(n).bid * (n+1)
  t
  
def part1: Long =
  scoreHands(getHands(false).sorted)

def part2: Long =
  scoreHands(getHands(true).sorted)

@main def hello: Unit =
  println("Part 1: " + part1)
  println("Part 2: " + part2)


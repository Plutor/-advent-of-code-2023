import scala.io.Source
import scala.collection.mutable._

def getData: List[String] =
  //Source.fromFile("testdata").getLines.toList
  Source.fromFile("data").getLines.toList

class Tile(val x: Int, val y: Int, val neigh: List[(Int, Int)], var dist: Int = -1):
  override def toString: String =
    s"neighbors=$neigh. dist=$dist"
end Tile

def getTiles: Map[(Int,Int),Tile] =
  val d = getData
  var tiles = Map[(Int,Int),Tile]()
  for (y <- 0 to d.size-1) {
    for (x <- 0 to d(y).size-1) {
      d(y)(x) match {
        case '.' =>
        case 'S' => tiles((x,y)) = Tile(x,y,List((x,y+1),(x,y-1),(x+1,y),(x-1,y)), 0)
        case '|' => tiles((x,y)) = Tile(x,y,List((x,y+1),(x,y-1)))
        case '-' => tiles((x,y)) = Tile(x,y,List((x+1,y),(x-1,y)))
        case 'L' => tiles((x,y)) = Tile(x,y,List((x+1,y),(x,y-1)))
        case 'J' => tiles((x,y)) = Tile(x,y,List((x-1,y),(x,y-1)))
        case '7' => tiles((x,y)) = Tile(x,y,List((x-1,y),(x,y+1)))
        case 'F' => tiles((x,y)) = Tile(x,y,List((x+1,y),(x,y+1)))
      }
    }
  }
  tiles

def getStart(t: Map[(Int,Int),Tile]): Tile =
  t.find(_(1).dist == 0).get(1)

def findLongestDist: Long =
  var t = getTiles
  var toVisit = Queue[Tile](getStart(t))
  var max = -1
  while (!toVisit.isEmpty) {
    var ct = toVisit.dequeue
    ct.neigh.foreach(next =>
      t get next match {
        case Some(nt: Tile) =>
          if (nt.neigh contains (ct.x,ct.y)) {
            if (nt.dist == -1) {
              nt.dist = ct.dist + 1
              toVisit.enqueue(nt)
            } else if (nt.dist > max) {
              max = nt.dist
            }
          }
        case None =>
      })
  }
  max
  
def part1: Long =
  findLongestDist

def part2: Long =
  2

@main def hello: Unit =
  println("Part 1: " + part1)
  println("Part 2: " + part2)


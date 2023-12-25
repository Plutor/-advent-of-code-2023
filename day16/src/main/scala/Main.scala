import scala.io.Source
import scala.collection.mutable._

def getData: List[String] =
  //Source.fromFile("testdata").getLines.toList
  Source.fromFile("data").getLines.toList

class Contraption(var layout: List[String]):
  var visited = List.fill(layout.size)(List.fill(layout(0).size)(false))
  visited = visited.updated(0, visited(0).updated(0, true))
  // (x,y,direction), where direction is 0=N, 1=E, 2=S, 3=W
  var rays = Set[(Int, Int, Int)]()
  rays.add(layout(0)(0) match {
    case '-' => (0,0,1)
    case '.' => (0,0,1)
    case '/' => (0,0,0)
    case '\\' => (0,0,2)
    case '|' => (0,0,2)
  })

  def this(l: List[String], v: List[List[Boolean]], r: Set[(Int,Int,Int)]) =
    this(l)
    visited = v
    rays = r

  override def toString: String =
    (0 to visited.size-1).map(y =>
        (0 to visited(y).size-1).map(x =>
          if (layout(y)(x) != '.') layout(y)(x)
          else if (visited(y)(x)) '#'
          else '.'
        ).mkString).mkString("\n") +
    s"\nRays: $rays\nScore: $score"

  def score: Int =
    visited.map(r=>r.filter(b=>b).size).sum

  def next: Contraption =
    var nv = visited
    var nr = Set[(Int,Int,Int)]()
    rays.foreach(r =>
        r(2) match {
          case 0 =>  // N
            val col = layout.map(_(r(0))).mkString
            var end = col.lastIndexWhere("-/\\".contains(_), r(1)-1)
            if (end == -1) end = 0
            nv = nv.patch(end, nv.drop(end).take(r(1)-end).map(_.updated(r(0), true)), r(1)-end)
            if ("-\\".contains(layout(end)(r(0)))) nr = nr + ((r(0),end,3))
            if ("-/".contains(layout(end)(r(0)))) nr = nr + ((r(0),end,1))
          case 1 =>  // E
            var end = layout(r(1)).indexWhere("|/\\".contains(_), r(0)+1)
            if (end == -1) end = layout(r(1)).size-1
            nv = nv.updated(r(1), nv(r(1)).patch(r(0)+1, List.fill(end-r(0))(true), end-r(0)))
            if ("|\\".contains(layout(r(1))(end))) nr = nr + ((end,r(1),2))
            if ("|/".contains(layout(r(1))(end))) nr = nr + ((end,r(1),0))
          case 2 =>  // S
            val col = layout.map(_(r(0))).mkString
            var end = col.indexWhere("-/\\".contains(_), r(1)+1)
            if (end == -1) end = layout(r(1)).size-1
            nv = nv.patch(r(1)+1, nv.drop(r(1)+1).take(end-r(1)).map(_.updated(r(0), true)), end-r(1))
            if ("-\\".contains(layout(end)(r(0)))) nr = nr + ((r(0),end,1))
            if ("-/".contains(layout(end)(r(0)))) nr = nr + ((r(0),end,3))
          case _ =>  // W
            var end = layout(r(1)).lastIndexWhere("|/\\".contains(_), r(0)-1)
            if (end == -1) end = 0
            nv = nv.updated(r(1), nv(r(1)).patch(end, List.fill(r(0)-end)(true), r(0)-end))
            if ("|\\".contains(layout(r(1))(end))) nr = nr + ((end,r(1),0))
            if ("|/".contains(layout(r(1))(end))) nr = nr + ((end,r(1),2))
        })
    Contraption(layout, nv, nr)
end Contraption

def part1: Long =
  var c = new Contraption(getData)
  var hist = Set[String]()
  println(c)
  while (!hist.contains(c.toString)) {
    hist.add(c.toString)
    c = c.next
    println(c)
  }
  c.score
  // 7259 is too high

def part2: Long =
  2

@main def hello: Unit =
  println("Part 1: " + part1)
  println("Part 2: " + part2)


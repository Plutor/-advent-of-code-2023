import scala.io.Source
import scala.collection.mutable._

def getData: List[String] =
  //Source.fromFile("testdata").getLines.toList
  Source.fromFile("data").getLines.toList

def getSteps: List[String] =
  getData.map(_.split(",")).flatten

def foldHash(before: Int, c: Char): Int =
  ((before + c.toInt)*17) % 256

def part1: Long =
  getSteps.map(_.toList.foldLeft(0)(foldHash)).sum

def followSteps: List[List[(String, Int)]] =
  var boxes = List.fill(256)(List[(String, Int)]())
  getSteps.foreach(st =>
    if (st.last == '-') {
      val b = st.init.foldLeft(0)(foldHash)
      boxes = boxes.updated(b, boxes(b).filter(_(0) != st.init))
    } else {
      val kn = st.split('=')
      val b = kn(0).foldLeft(0)(foldHash)
      if (boxes(b).exists(_(0) == kn(0))) {
        boxes = boxes.updated(b, boxes(b).updated(boxes(b).indexWhere((s:String, v:Int) => s == kn(0)), (kn(0), kn(1).toInt)))
      } else {
        boxes = boxes.updated(b, boxes(b) :+ (kn(0), kn(1).toInt))
      }
    }
  )
  boxes

def scoreBox(l: List[(String, Int)], bi: Int): Int =
  l.zipWithIndex.map((lens, li) => (bi+1)*lens(1)*(li+1)).sum
  
def part2: Long =
  followSteps.zipWithIndex.map((x,i) => scoreBox(x,i)).sum

@main def hello: Unit =
  println("Part 1: " + part1)
  println("Part 2: " + part2)


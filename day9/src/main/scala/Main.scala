import scala.io.Source

def getData: Array[String] =
  Source.fromFile("data").getLines.toArray

def getHistories: Array[Array[Long]] =
  getData.map(_.split(" ").map(_.toLong))

def getHistoryNext(h: Array[Long]) =
  var diffs, lastdiffs = h
  for (n <- h.size-1 to 0 by -1) {
    lastdiffs = diffs.clone()
    for (m <- 0 to n-1) {
      diffs(m) = lastdiffs(m+1) - lastdiffs(m)
    }
  }
  diffs.sum

def getHistoryPrev(h: Array[Long]) =
  var diffs, lastdiffs = h
  for (n <- 0 to h.size-1) {
    lastdiffs = diffs.clone()
    for (m <- n to h.size-2) {
      diffs(m+1) = lastdiffs(m) - lastdiffs(m+1)
    }
  }
  diffs.sum

def part1: Long =
  getHistories.map(getHistoryNext).sum

def part2: Long =
  getHistories.map(getHistoryPrev).sum

@main def hello: Unit =
  println("Part 1: " + part1)
  println("Part 2: " + part2)


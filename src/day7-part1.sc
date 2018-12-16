import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

val input =
  """Step D must be finished before step L can begin.
    |Step B must be finished before step G can begin.
    |Step N must be finished before step Z can begin.
    |Step V must be finished before step Y can begin.
    |Step G must be finished before step R can begin.
    |Step E must be finished before step L can begin.
    |Step O must be finished before step W can begin.
    |Step Q must be finished before step X can begin.
    |Step Y must be finished before step J can begin.
    |Step W must be finished before step R can begin.
    |Step H must be finished before step P can begin.
    |Step L must be finished before step P can begin.
    |Step X must be finished before step T can begin.
    |Step S must be finished before step K can begin.
    |Step U must be finished before step R can begin.
    |Step P must be finished before step Z can begin.
    |Step A must be finished before step Z can begin.
    |Step M must be finished before step F can begin.
    |Step Z must be finished before step K can begin.
    |Step J must be finished before step K can begin.
    |Step I must be finished before step C can begin.
    |Step K must be finished before step C can begin.
    |Step R must be finished before step C can begin.
    |Step T must be finished before step F can begin.
    |Step F must be finished before step C can begin.
    |Step E must be finished before step C can begin.
    |Step H must be finished before step S can begin.
    |Step X must be finished before step K can begin.
    |Step E must be finished before step Y can begin.
    |Step I must be finished before step T can begin.
    |Step T must be finished before step C can begin.
    |Step R must be finished before step T can begin.
    |Step X must be finished before step U can begin.
    |Step I must be finished before step R can begin.
    |Step I must be finished before step K can begin.
    |Step L must be finished before step A can begin.
    |Step P must be finished before step C can begin.
    |Step D must be finished before step S can begin.
    |Step Q must be finished before step A can begin.
    |Step N must be finished before step G can begin.
    |Step W must be finished before step P can begin.
    |Step B must be finished before step E can begin.
    |Step M must be finished before step T can begin.
    |Step L must be finished before step C can begin.
    |Step E must be finished before step F can begin.
    |Step M must be finished before step I can begin.
    |Step M must be finished before step Z can begin.
    |Step U must be finished before step F can begin.
    |Step W must be finished before step K can begin.
    |Step E must be finished before step W can begin.
    |Step B must be finished before step W can begin.
    |Step A must be finished before step I can begin.
    |Step M must be finished before step K can begin.
    |Step X must be finished before step F can begin.
    |Step O must be finished before step Z can begin.
    |Step W must be finished before step C can begin.
    |Step Q must be finished before step M can begin.
    |Step K must be finished before step R can begin.
    |Step Y must be finished before step C can begin.
    |Step A must be finished before step T can begin.
    |Step G must be finished before step L can begin.
    |Step G must be finished before step A can begin.
    |Step J must be finished before step I can begin.
    |Step N must be finished before step R can begin.
    |Step D must be finished before step W can begin.
    |Step L must be finished before step R can begin.
    |Step Y must be finished before step P can begin.
    |Step S must be finished before step M can begin.
    |Step O must be finished before step I can begin.
    |Step Z must be finished before step J can begin.
    |Step H must be finished before step M can begin.
    |Step L must be finished before step K can begin.
    |Step U must be finished before step M can begin.
    |Step G must be finished before step T can begin.
    |Step O must be finished before step L can begin.
    |Step Z must be finished before step R can begin.
    |Step N must be finished before step E can begin.
    |Step U must be finished before step I can begin.
    |Step G must be finished before step Q can begin.
    |Step H must be finished before step R can begin.
    |Step U must be finished before step C can begin.
    |Step L must be finished before step U can begin.
    |Step H must be finished before step Z can begin.
    |Step P must be finished before step F can begin.
    |Step I must be finished before step F can begin.
    |Step B must be finished before step N can begin.
    |Step J must be finished before step R can begin.
    |Step O must be finished before step S can begin.
    |Step Y must be finished before step T can begin.
    |Step G must be finished before step S can begin.
    |Step N must be finished before step O can begin.
    |Step Y must be finished before step X can begin.
    |Step B must be finished before step X can begin.
    |Step A must be finished before step F can begin.
    |Step Z must be finished before step I can begin.
    |Step K must be finished before step F can begin.
    |Step J must be finished before step C can begin.
    |Step D must be finished before step G can begin.
    |Step P must be finished before step M can begin.
    |Step K must be finished before step T can begin.
    |Step X must be finished before step M can begin.""".stripMargin

val r = """^Step ([A-Z]) must be finished before step ([A-Z]) can begin.$""".r.anchored

val deps = input.split("\n").map {
  case r(before, after) => (before, after)
} toSet

val getNodesBlockedBy: Map[String, Set[String]] = deps.groupBy(_._1).mapValues(_.map(_._2))

val getNodesBlocking: Map[String, Set[String]] = deps.groupBy(_._2).mapValues(_.map(_._1))

val graphviz = deps.map{
  case (from, to) => s""""$from" -> "$to""""
} mkString("\n")

val nodes = (deps.map(_._1) ++ deps.map(_._2)).toSet.toList.sorted

val rootNodes = nodes.filter(n => getNodesBlocking.get(n).isEmpty)

var finished: Seq[String] = Seq.empty[String]
var queue: Seq[String] = rootNodes

while (queue.nonEmpty) {
  val cur = queue.filter(n => getNodesBlocking.getOrElse(n, Set.empty[String]).forall(finished contains _)).min
  queue = queue.filterNot(_ == cur)
  finished = finished :+ cur
  println("Current is " ++ cur ++ " queue is " ++ queue.toString)
  queue = queue ++ getNodesBlockedBy.getOrElse(cur, Seq.empty[String])
}

finished.mkString("")
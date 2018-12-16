val input =
  """264, 340
    |308, 156
    |252, 127
    |65, 75
    |102, 291
    |47, 67
    |83, 44
    |313, 307
    |159, 48
    |84, 59
    |263, 248
    |188, 258
    |312, 240
    |59, 173
    |191, 130
    |155, 266
    |252, 119
    |108, 299
    |50, 84
    |172, 227
    |226, 159
    |262, 177
    |233, 137
    |140, 211
    |108, 175
    |278, 255
    |259, 209
    |233, 62
    |44, 341
    |58, 175
    |252, 74
    |232, 63
    |176, 119
    |209, 334
    |103, 112
    |155, 94
    |253, 255
    |169, 87
    |135, 342
    |55, 187
    |313, 338
    |210, 63
    |237, 321
    |171, 143
    |63, 238
    |79, 132
    |135, 113
    |310, 294
    |289, 184
    |56, 259""".stripMargin

val coords = input.split("\n").map { l =>
  val List(x, y) = l.split(", ").toList
  (x.toInt, y.toInt)
}

def dist(x: Int, y: Int, x2: Int, y2: Int): Int = Math.abs(x2-x) + Math.abs(y2-y)

val allDists = for {
  c1 <- coords
  c2 <- coords
  d = dist(c1._1, c1._2, c2._1, c2._2) if c1 != c2
} yield d

val maxDist = allDists.max

val minX = coords.map(_._1).min
val maxX = coords.map(_._1).max

val minY = coords.map(_._2).min
val maxY = coords.map(_._2).max

val width = maxX - minX
val height = maxY - minY

val cells = for {
  i <- minY to maxY
  j <- minX to maxX
} yield (j,i)

val cellsWithClosest = cells.map {
  case (x,y) =>
    val groupedByDistanceSorted = coords.map(c => (c, dist(x,y, c._1, c._2))).groupBy(_._2).toList.sortBy(_._1)
    if(groupedByDistanceSorted.head._2.length > 1){
      None
    } else {
      Some(groupedByDistanceSorted.head._2.head._1)
    }
}

cellsWithClosest.filter(_.isDefined).groupBy(identity).map(_._2.length).toList.sorted(Ordering.Int.reverse)(1)
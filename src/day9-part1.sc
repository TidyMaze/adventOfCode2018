import scala.language.postfixOps

val nbPlayers = 9
val lastValue = 25

val initialScores = Map.empty[Int, Int]

def mod(a: Int, b: Int) = {
  var curA = a
  if(a >= b){
    while(curA >= b) curA -= b
  } else if(a < 0){
    while(curA < 0) curA += b
  }
  curA
}

def insert[A](xs: Seq[A], at: Int, el: A): Seq[A] = xs.patch(at, Seq(el), 0)
def delete[A](xs: Seq[A], at: Int): Seq[A] = xs.zipWithIndex.filter(_._2 != at).map(_._1)

val res = (0 until lastValue).scanLeft((1, 0, initialScores, 0, Seq(0))){
    case (acc@(turn, player, scores, marble, circle), cur) => {
      val currentIndex = circle indexOf marble
      val (newCurrentMarble, newCircle, newScores) =
        if(turn > 0 && turn % 23 == 0){
          val deleteAt = mod(currentIndex - 7, circle.length)
          (circle(mod(deleteAt-1, circle.length-1)), delete(circle, deleteAt), scores + (player -> (scores.getOrElse(player, 0) + 23 + circle(deleteAt))))
        } else {
          (turn, insert(circle, mod(currentIndex + 2, circle.length), turn), scores)
        }

      (turn + 1, mod(player + 1, nbPlayers), newScores, newCurrentMarble, newCircle)
    }
  }

res.foreach {
  case (turn, player, scores, currentMarble, circle) =>
    println(s"$turn by $player: max: ${scores.toList.sortBy(_._2).reverse.headOption.getOrElse((-1, 0))} current $currentMarble, circleSize: ${circle.length}")
}

res.last._3.maxBy(_._2)._2
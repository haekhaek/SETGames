package model

object EloScore{
  val k = 10

  def calcEloScore(scoreA: Int, scoreB: Int, outCome: String): Int ={
    val expectedScore = 1/(1+ math.pow(10, (scoreB - scoreA)/400))
    val s: Double = outCome match{
      case "won" => 1
      case "loss" => 0
      case "even" => 0.5
      case "_" => 0 //???
    }
    return (scoreA + k* (s - expectedScore)).toInt
  }
}
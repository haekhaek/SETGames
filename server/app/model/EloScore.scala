package model

object EloScore{
  val k = 10

  def calcEloScore(scoreA: Int, scoreB: Int, outCome: String): (Int, Int) ={
    val expectedScoreA = 1/(1+ math.pow(10, (scoreB - scoreA)/400))
    val expectedScoreB = 1/(1+ math.pow(10, (scoreA - scoreB)/400))
    val s: Double = outCome match{
      case "won" => 1
      case "loss" => 0
      case "even" => 0.5
      case "_" => 0 //???
    }
    val sA: Int = (scoreA + k* (s - expectedScoreA)).toInt
    val sB: Int = (scoreB + k* ((1-s) - expectedScoreB)).toInt
    println("EA: " + expectedScoreA + " EB: " + expectedScoreB)
    println("new scoreA: " + sA + " newScoreB: " + sB )
    return (sA, sB)
  }
}
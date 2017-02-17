package model
import shared.GameState
import play.api.Logger

object EloScore{
  val k = 10

  def calcEloScore(scoreA: Int, scoreB: Int, outCome: String): (Int, Int) ={
    val expectedScoreA = 1/(1+ math.pow(10, (scoreB - scoreA)/400))
    val expectedScoreB = 1/(1+ math.pow(10, (scoreA - scoreB)/400))
    val s: Double = GameState.withName(outCome) match {
        case GameState.WON => 1
        case GameState.LOST => 0
        case GameState.EVEN => 0.5
        case _ => 0   
      }
    val sA: Int = (scoreA + k* (s - expectedScoreA)).toInt
    val sB: Int = (scoreB + k* ((1-s) - expectedScoreB)).toInt
    Logger.debug(s"EA: $expectedScoreA EB: $expectedScoreB")
    Logger.debug(s"new scoreA: $sA newScoreB: $sB" )
    return (sA, sB)
  }
}
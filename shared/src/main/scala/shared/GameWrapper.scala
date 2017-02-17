package shared

trait GameWrapper {

    def getAvailableActionWrappers(): List[ActionWrapper]
    
    def updateStateWrapper(action: ActionWrapper): StateWrapper
    
    def currentState : StateWrapper

}

case class ActionWrapper(val data : Iterable[Int])

case class StateWrapper(val gameState: String, val playerLabel : Char, val field : Iterable[Iterable[Char]])

object GameState extends Enumeration {
  val WON, LOST, EVEN, ONGOING = Value
}
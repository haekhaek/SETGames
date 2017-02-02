package model

import shared.{GameWrapper, ActionWrapper, StateWrapper}

trait FieldStupidButton extends Field{
  override def playField: Array[Array[Char]] = Array.fill(1, 1)('_')
}

trait UpdateStupidButton extends Update[Int] {
  override var availableActions: List[Action[Int]] = List()
  override def update(action: Action[Int], f: Array[Array[Char]], p: Player, s:State) : State = s
}

class StupidButtonGame extends GameWrapper {

    val game = new Game[Int] with FieldStupidButton with UpdateStupidButton {}

    def getAvailableActionWrappers(): List[ActionWrapper] = List()
    
    def updateStateWrapper(action: ActionWrapper): StateWrapper = {
        val state = game.updateGameState(Action(42))
        StateWrapper(state.gameState, game.player.playerLabel, List())
    }
    
    def currentState : StateWrapper = StateWrapper(game.state.gameState, game.player.playerLabel, List())

}
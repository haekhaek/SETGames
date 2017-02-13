/**
  * Created by sabrina on 01/02/2017.
  */

package model

import shared.{GameWrapper, ActionWrapper, StateWrapper}

trait Game[T] {
  this : Field with Update[T] =>
  var state: State = new State
  var player: Player = new Player
  var field = this.playField //this = Field

  def getAvailableActions(): List[Action[T]] = this.availableActions  //this = Update
  def updateGameState(action: Action[T]): State = {
    state = this.update(action, field, player, state)     //this = Update
    if (state.gameState == "ongoing"){
      player.switch()
    }
    else if (state.gameState == "won"){
      println("Game over. Player " + player.playerLabel + " has won." )
    }
    else{
      println("Game over. Draw!")
    }
    return(state)
  }
}


trait Field{
  def playField: Array[Array[Char]]
}

trait FieldTicTacToe extends Field{
  override def playField: Array[Array[Char]] = Array.fill(3, 3)('_')
}

trait FieldFourWins extends Field{
  override def playField: Array[Array[Char]] = Array.fill(6, 7)('_')
}


trait Update[T]{
  var availableActions: List[Action[T]]
  def update(action: Action[T], f: Array[Array[Char]], p:Player, s:State): State
  def checkWin(lastRow: Int, lastCol: Int, maxRow: Int, maxCol: Int, noRequired: Int, f:Array[Array[Char]]): String ={
    var gameState = "ongoing"
    val curPlayer = f(lastRow)(lastCol)
    var inLine = 1
    val min = -(noRequired-1)
    val max = noRequired-2

    //check row
    for(i <- min to max){
      if(lastCol+i >= 0 && lastCol+i+1 <=maxCol){
        if (f(lastRow)(lastCol+i) == curPlayer && f(lastRow)(lastCol+i+1) == curPlayer) {
          inLine += 1
        }
      }
    }
    if (inLine >=noRequired){
      gameState = "won"
      return(gameState)
    }

    //check column
    inLine = 1
    for(i <- min to max){
      if(lastRow+i >= 0 && lastRow+i+1 <=maxRow){
        if (f(lastRow+i)(lastCol) == curPlayer && f(lastRow+i+1)(lastCol) == curPlayer){
          inLine +=1
        }
      }
    }
    if (inLine >= noRequired){
      gameState = "won"
      return(gameState)
    }

    //check left bottom to right top
    inLine = 1
    for(i <- min to max){
      if(lastRow+i >= 0 &&lastCol+i >= 0 && lastRow+i+1 <= maxRow && lastCol+i+1 <= maxCol){
        if (f(lastRow+i)(lastCol+i) == curPlayer && f(lastRow+i+1)(lastCol+i+1) == curPlayer){
          inLine +=1
        }
      }
    }
    if (inLine >=noRequired){
      gameState = "won"
      return(gameState)
    }

    //check left top to right bottom
    inLine = 1
    for(i <- min to max){
      if(lastRow+i >= 0 && lastCol-i-1 >= 0 && lastRow+i+1 <=maxRow &&lastCol-i <= maxCol) {
        if (f(lastRow+i)(lastCol-i) == curPlayer && f(lastRow+i+1)(lastCol-i-1) == curPlayer) {
          inLine += 1
        }
      }
    }
    if (inLine >=noRequired){
      gameState = "won"
      return(gameState)
    }

    //no winner or loser
    if (availableActions.isEmpty) {
      gameState = "even"
      return(gameState)
    }

    else {
      gameState = "ongoing"
      return(gameState)
    }
  }
}

trait UpdateTicTacToe extends Update[Location]{
  override var availableActions = (for (i <- 0 to 2; j <- 0 to 2) yield (Action(Location(i, j)))).toList
  override def update(action: Action[Location], f: Array[Array[Char]], p: Player, s:State): State = {
    if (availableActions.contains(action)) {
      val r = action.data.row
      val c = action.data.col
      f(r)(c) = p.playerLabel
      availableActions = availableActions.filter(_ != action)
      s.gameState = checkWin(r, c, f)
      return s
    }

    else{
      println("Field not available, please try again!")
      return s
    }
  }
  def checkWin = super.checkWin(_:Int, _: Int, 2, 2, 3, _:Array[Array[Char]])
}


trait UpdateFourWins extends Update[Column]{
  private var curRow = Array.fill(7)(0) //per column, remember last free row
  override var availableActions = (for (c <- 0 to 6) yield (Action(Column(c)))).toList
  override def update(action: Action[Column], f: Array[Array[Char]], p: Player, s: State): State = {
    if (availableActions.contains(action)) {
      val c = action.data.col
      f(curRow(c))(c) = p.playerLabel
      if (curRow(c) >= 5){
        availableActions = availableActions.filter(_ != action) //if column is full, remove from available actions
      }
      s.gameState = checkWin(curRow(c), c, f)
      curRow(c) += 1

      return s
    }
    else{
      println("Column already full, please try again!")
      return s
    }
  }
  def checkWin = super.checkWin(_:Int, _: Int, 5, 6, 4, _:Array[Array[Char]])
}


case class Action[T](data: T) //für Tic Tac Toe: Location, für 4 gewinnt: Spalte

class State{
  var gameState: String  = "ongoing"//won, lost, even, ongoing
}

class Player{
  var playerLabel: Char = 'X'
  def switch() = {
    var newLabel: Char = playerLabel match {
      case 'X' => 'O'
      case 'O' => 'X'
      case _ => ' '
    }
    this.playerLabel = newLabel
  }
}

case class Location(r: Int, c: Int){
  val row: Int = r
  val col: Int = c
}

case class Column(c: Int){
  val col: Int = c
}

class TicTacToeGame extends GameWrapper {

    val game = new Game[Location] with UpdateTicTacToe with FieldTicTacToe {}

    def getAvailableActionWrappers(): List[ActionWrapper] =
        game.getAvailableActions.map(a => ActionWrapper(List(a.data.r,a.data.c))).toList
    
    def updateStateWrapper(action: ActionWrapper): StateWrapper = {
        val unwrappedAction = Action(Location(action.data.toList(0),action.data.toList(1)))
        game.updateGameState(unwrappedAction)
        currentState
    }
    
    def currentState : StateWrapper = {
        StateWrapper(game.state.gameState, game.player.playerLabel,
            game.field.toList.map(a => a.toList).toList)
    }

}

class FourWinsGame extends GameWrapper {
    val game = new Game[Column] with UpdateFourWins with FieldFourWins {}

    def getAvailableActionWrappers(): List[ActionWrapper] =
        game.getAvailableActions.map(a => ActionWrapper(List(a.data.c))).toList

    def updateStateWrapper(action: ActionWrapper): StateWrapper = {
        val unwrappedAction = Action(Column(action.data.toList(0)))
        game.updateGameState(unwrappedAction)
        currentState
    }

    def currentState : StateWrapper = {
        StateWrapper(game.state.gameState, game.player.playerLabel,
            game.field.toList.map(a => a.toList).toList)
    }

}

object main extends App{
  println("******NEW GAME*********")
  var g = new Game[Location] with UpdateTicTacToe with FieldTicTacToe {}
  println("player: " + g.player.playerLabel)
  g.updateGameState(Action(Location(1,1))) //X
  println("field: " + g.field(1)(1))
  println("state: " + g.state.gameState)
  println("new player: " + g.player.playerLabel)
  g.updateGameState(Action(Location(2, 0))) //O
  println("state: " + g.state.gameState)
  println("new player: " + g.player.playerLabel)
  g.updateGameState(Action(Location(1, 0))) //X
  println("state: " + g.state.gameState)
  println("new player: " + g.player.playerLabel)
  g.updateGameState(Action(Location(2, 2))) //O
  println("state: " + g.state.gameState)
  println("new player: " + g.player.playerLabel)
  g.updateGameState(Action(Location(0,2))) //X
  println("state: " + g.state.gameState)
  println("new player: " + g.player.playerLabel)
  g.updateGameState(Action(Location(1,2))) //O
  println("state: " + g.state.gameState)
  println("new player: " + g.player.playerLabel)
  g.updateGameState(Action(Location(2,1))) //X
  println("state: " + g.state.gameState)
  println("new player: " + g.player.playerLabel)
  g.updateGameState(Action(Location(0,1))) //O
  println("state: " + g.state.gameState)
  println("new player: " + g.player.playerLabel)
  g.updateGameState(Action(Location(0,0))) //X
  println("state: " + g.state.gameState)
  println("new player: " + g.player.playerLabel)

  println("******NEW GAME*********")
  var newGame2 = new Game[Location] with UpdateTicTacToe with FieldTicTacToe {}
  println("player: " + newGame2.player.playerLabel)
  newGame2.updateGameState(Action(Location(1,1))) //X
  println("field: " + newGame2.field(1)(1))
  println("state: " + newGame2.state.gameState)
  println("new player: " + newGame2.player.playerLabel)
  newGame2.updateGameState(Action(Location(2, 1))) //O
  println("state: " + newGame2.state.gameState)
  println("new player: " + newGame2.player.playerLabel)
  newGame2.updateGameState(Action(Location(0, 0))) //X
  println("state: " + newGame2.state.gameState)
  println("new player: " + newGame2.player.playerLabel)
  newGame2.updateGameState(Action(Location(2, 0))) //O
  println("state: " + newGame2.state.gameState)
  println("new player: " + newGame2.player.playerLabel)
  newGame2.updateGameState(Action(Location(2,2))) //X
  println("state: " + newGame2.state.gameState)
  println("new player: " + newGame2.player.playerLabel)
}
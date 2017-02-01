package model

trait Game[T] {
  var state: State
  var player: Player
  var field: Array[Array[Char]]
  def getAvailableActions(): List[Action[T]]
  def update(action: Action[T]): State
}

trait TicTacToe extends Game[Location]{
  override var state = new State() //ongoing
  override var player = new Player()
  override var field = Array.fill(3, 3)(' ')
  private var availableActions = (for (i <- 0 to 2; j <- 0 to 2) yield (Action(Location(i, j)))).toList
  override def getAvailableActions(): List[Action[Location]] = availableActions

  override def update(action: Action[Location]): State = {
    if (availableActions.contains(action)) {
      val r = action.data.row
      val c = action.data.col
      field(r)(c) = this.player.playerLabel
      availableActions = availableActions.filter(_ != action)
      updateState(r, c)
    }
    else{
      println("Field not available, please try again!")
      return(state)
    }
  }
  def updateState(lastRow: Int, lastCol: Int): State = {
    val curPlayer = this.player.playerLabel
    var inLine = 1
    //check row
    for(i <- -2 to 1){
      if(lastCol+i >= 0 && lastCol+i+1 <=2){
        if (field(lastRow)(lastCol+i) == curPlayer && field(lastRow)(lastCol+i+1) == curPlayer) {
          inLine += 1
        }
      }
    }
    if (inLine >=3){
      state.gameState = "won"
      return(state)
    }

    //check column
    inLine = 1
    for(i <- -2 to 1){
      if(lastRow+i >= 0 && lastRow+i+1 <=2){
        if (field(lastRow+i)(lastCol) == curPlayer && field(lastRow+i+1)(lastCol) == curPlayer){
          inLine +=1
        }
      }
    }
    if (inLine >=3){
      state.gameState = "won"
      return(state)
    }

    //check left bottom to right top
    inLine = 1
    for(i <- -2 to 1){
      if(lastRow+i >= 0 &&lastCol+i >= 0 && lastRow+i+1 <=2 && lastCol+i+1 <=2){
        if (field(lastRow+i)(lastCol+i) == curPlayer && field(lastRow+i+1)(lastCol+i+1) == curPlayer){
          inLine +=1
        }
      }
    }
    if (inLine >=3){
      state.gameState = "won"
      return(state)
    }

    //check left top to right bottom
    inLine = 1
    for(i <- -2 to 1){
      if(lastRow+i >= 0 && lastCol-i-1 >= 0 && lastRow+i+1 <=2 &&lastCol-i <= 2) {
        if (field(lastRow+i)(lastCol-i) == curPlayer && field(lastRow+i+1)(lastCol-i-1) == curPlayer) {
          inLine += 1
        }
      }
    }
    if (inLine >=3){
      state.gameState = "won"
      return(state)
    }

    //no winner or loser
    if (availableActions.isEmpty) {
      state.gameState = "even"
      return(state)
    }

    else {
      state.gameState = "ongoing"
      player.switch() //switch player
      return(state)
    }
  }
}




trait FourWins extends Game[Column]{
  override var state = new State() //ongoing
  override var player = new Player()
  override var field = Array.fill(6, 7)(' ') //0,0 is bottom left!
  private var curRow = Array.fill(7)(0) //per column, remember last free row

  private var availableActions = (for (c <- 0 to 6) yield (Action(Column(c)))).toList
  override def getAvailableActions(): List[Action[Column]] = availableActions

  override def update(action: Action[Column]): State = {
    if (availableActions.contains(action)) {
      val c = action.data.col
      field(curRow(c))(c) = this.player.playerLabel
      if (curRow(c) >= 5){
        availableActions = availableActions.filter(_ != action) //if column is full, remove from available actions
      }
      updateState(curRow(c), c)
      curRow(c) += 1
      return(state)
    }
    else{
      printf("Column already full, please try again!")
      return(state)
    }
  }

  def updateState(lastRow: Int, lastCol:Int): State = {
    val curPlayer = this.player.playerLabel
    var inLine = 1

    //check row
    for(i <- -3 to 2){
      if(lastCol+i >= 0 && lastCol+i <=6)
      if (field(lastRow)(lastCol+i) == curPlayer && field(lastRow)(lastCol+i+1) == curPlayer){
        inLine +=1
      }
    }
    if (inLine >=4){
      state.gameState = "won"
      return(state)
    }

    //check column
    inLine = 1
    for(i <- -3 to 2){
      if (field(lastRow+i)(lastCol) == curPlayer && field(lastRow+i+1)(lastCol) == curPlayer){
        inLine +=1
      }
    }
    if (inLine >=4){
      state.gameState = "won"
      return(state)
    }

    //check left bottom to right top
    inLine = 1
    for(i <- -3 to 2){
      if (field(lastRow+i)(lastCol+i) == curPlayer && field(lastRow+i+1)(lastCol+i) == curPlayer){
        inLine +=1
      }
    }
    if (inLine >=4){
      state.gameState = "won"
      return(state)
    }

    //check left top to right bottom
    inLine = 1
    for(i <- -3 to 2){
      if (field(lastRow+i)(lastCol-i) == curPlayer && field(lastRow+i+1)(lastCol-i-1) == curPlayer){
        inLine +=1
      }
    }
    if (inLine >=4){
      state.gameState = "won"
      return(state)
    }

    //no winner or loser
    if (availableActions.isEmpty) {
      state.gameState = "even"
      return(state)
    }

    else {
      state.gameState = "ongoing"
      player.switch() //switch player
      return(state)
    }
  }
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

case class Column( c: Int){
  val col: Int = c
}



object main extends App{
  var newGame = new TicTacToe {}
  println("player: " + newGame.player.playerLabel)
  newGame.update(Action(Location(1,1))) //X
  println("field: " + newGame.field(1)(1))
  println("state: " + newGame.state.gameState)
  println("new player: " + newGame.player.playerLabel)
  newGame.update(Action(Location(2, 0))) //O
  println("state: " + newGame.state.gameState)
  println("new player: " + newGame.player.playerLabel)
  newGame.update(Action(Location(1, 0))) //X
  println("state: " + newGame.state.gameState)
  println("new player: " + newGame.player.playerLabel)
  newGame.update(Action(Location(2, 2))) //O
  println("state: " + newGame.state.gameState)
  println("new player: " + newGame.player.playerLabel)
  newGame.update(Action(Location(0,2))) //X
  println("state: " + newGame.state.gameState)
  println("new player: " + newGame.player.playerLabel)
  newGame.update(Action(Location(1,2))) //O
  println("state: " + newGame.state.gameState)
  println("new player: " + newGame.player.playerLabel)
  newGame.update(Action(Location(2,1))) //X
  println("state: " + newGame.state.gameState)
  println("new player: " + newGame.player.playerLabel)
  newGame.update(Action(Location(0,1))) //O
  println("state: " + newGame.state.gameState)
  println("new player: " + newGame.player.playerLabel)
  newGame.update(Action(Location(0,0))) //X
  println("state: " + newGame.state.gameState)
  println("new player: " + newGame.player.playerLabel)


  var newGame2 = new TicTacToe {}
  println("player: " + newGame2.player.playerLabel)
  newGame2.update(Action(Location(1,1))) //X
  println("field: " + newGame2.field(1)(1))
  println("state: " + newGame2.state.gameState)
  println("new player: " + newGame2.player.playerLabel)
  newGame2.update(Action(Location(2, 1))) //O
  println("state: " + newGame2.state.gameState)
  println("new player: " + newGame2.player.playerLabel)
  newGame2.update(Action(Location(0, 0))) //X
  println("state: " + newGame2.state.gameState)
  println("new player: " + newGame2.player.playerLabel)
  newGame2.update(Action(Location(2, 0))) //O
  println("state: " + newGame2.state.gameState)
  println("new player: " + newGame2.player.playerLabel)
  newGame2.update(Action(Location(2,2))) //X
  println("state: " + newGame2.state.gameState)
  println("new player: " + newGame2.player.playerLabel)
}
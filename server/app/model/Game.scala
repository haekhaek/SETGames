/**
  * Created by sabrina on 01/02/2017.
  */

package model
import play.api.Logger
import shared.{GameWrapper, ActionWrapper, StateWrapper, GameState}

trait Game[T] {
  this : Field with Update[T] =>
  var state: State = new State
  var player: Player = new Player
  var field = this.playField //this = Field
  var field2 = this.playField2 //competitor field for schiffe versenken
  def updateGameState(action: Action[T]): State = {
    state = this.update(action, field, field2, player, state)     //this = Update
    return(state)
  }
}

trait Field{
  def playField: Array[Array[Char]]
  def playField2: Array[Array[Char]] = Array()
}

trait FieldTicTacToe extends Field{
  override def playField: Array[Array[Char]] = Array.fill(3, 3)('_')
}

trait FieldFourWins extends Field{
  override def playField: Array[Array[Char]] = Array.fill(6, 7)('_')
}

trait FieldBattleship extends Field {
  override def playField: Array[Array[Char]] = initializeField() //player X'es own field
  override def playField2: Array[Array[Char]] = initializeField()

  def initializeField(): Array[Array[Char]] = {
    var f = Array.fill(10, 10)('_')
    var availableBoxes: List[Location] = (for (i <- 0 to 9; j <- 0 to 9) yield (Location(i, j))).toList
    val shipSizes: List[Int] = List(5, 4, 3, 2, 2)
    var canSet: Boolean = false

    for (shipSize <- shipSizes){
      Logger.debug(s"Set ship of size $shipSize")
      while(!canSet){
        val res = setShip(shipSize, availableBoxes, f)
        canSet = res._1
        if (canSet){
          availableBoxes = res._2
          Logger.debug(s"Set! :)")
        }
        else{
          Logger.debug(s"cannot set ship of size $shipSize")
        }
      }
      canSet = false
    }
    for (row <- f){
      for (elem <- row){
        print(elem + " ")
      }
    }
    return f
  }

  def setShip(size: Int, availableBoxes: List[Location], field: Array[Array[Char]]): (Boolean, List[Location]) = {
    var freeBoxes: List[Location] = availableBoxes
    val r = scala.util.Random
    val o = r.nextInt(2) //orientation 0 or 1
    var x: Int = 0
    var y: Int = 0

    if (o == 0){ //horizontal
      x = r.nextInt(10)
      y = r.nextInt(10-size) //may not start higher, otherwise overlaps with field border
    }
    else{//o==1, vertical
      x = r.nextInt(10)
      y = r.nextInt(10-size)
    }

    //boxes
    val s = size-1
    val shipBoxes: List[Location] = (for (i <- 0 to s*(1-o); j <- 0 to s*o) yield ((Location(x + i, y + j)))).toList
    val enclosingBoxes: List[Location] = shipBoxes
      .flatMap(
        pos => for(x <- -1 to 1; y <- -1 to 1) yield (Location(pos.row + x, pos.col + y))
      ).toSet.toList

    //test if ship can be set there 
    val canSet = shipBoxes.filter(shipBox => !freeBoxes.contains(shipBox)).isEmpty
    if(!canSet){
      return (false, availableBoxes)
    }else{
      shipBoxes.foreach(shipBox => {
        field(shipBox.row)(shipBox.col) = 'B'
      })

      enclosingBoxes.foreach(box => {
        freeBoxes = freeBoxes.filter(_ != box)
      })
      return (true, freeBoxes)
    }
  }
}

trait Update[T]{
  var availableActions: List[Action[T]] //player X
  //var availableActions2: List[Action[T]]//player O
  def getAvailableActions(): List[Action[T]] = this.availableActions
  def update(action: Action[T], f1: Array[Array[Char]], f2: Array[Array[Char]], p:Player, s:State): State
  def checkWin(lastRow: Int, lastCol: Int, maxRow: Int, maxCol: Int, noRequired: Int, f1:Array[Array[Char]]): GameState.Value ={
    var gameState = GameState.ONGOING
    val curPlayer = f1(lastRow)(lastCol)
    var inLine = 1
    val min = -(noRequired-1)
    val max = noRequired-2

    //check row
    for(i <- min to max){
      if(lastCol+i >= 0 && lastCol+i+1 <=maxCol){
        if (f1(lastRow)(lastCol+i) == curPlayer && f1(lastRow)(lastCol+i+1) == curPlayer) {
          inLine += 1
        }
      }
    }
    if (inLine >=noRequired){
      gameState = GameState.WON
      return(gameState)
    }

    //check column
    inLine = 1
    for(i <- min to max){
      if(lastRow+i >= 0 && lastRow+i+1 <=maxRow){
        if (f1(lastRow+i)(lastCol) == curPlayer && f1(lastRow+i+1)(lastCol) == curPlayer){
          inLine +=1
        }
      }
    }
    if (inLine >= noRequired){
      gameState = GameState.WON
      return(gameState)
    }

    //check left bottom to right top
    inLine = 1
    for(i <- min to max){
      if(lastRow+i >= 0 &&lastCol+i >= 0 && lastRow+i+1 <= maxRow && lastCol+i+1 <= maxCol){
        if (f1(lastRow+i)(lastCol+i) == curPlayer && f1(lastRow+i+1)(lastCol+i+1) == curPlayer){
          inLine +=1
        }
      }
    }
    if (inLine >=noRequired){
      gameState = GameState.WON
      return(gameState)
    }

    //check left top to right bottom
    inLine = 1
    for(i <- min to max){
      if(lastRow+i >= 0 && lastCol-i-1 >= 0 && lastRow+i+1 <=maxRow &&lastCol-i <= maxCol) {
        if (f1(lastRow+i)(lastCol-i) == curPlayer && f1(lastRow+i+1)(lastCol-i-1) == curPlayer) {
          inLine += 1
        }
      }
    }
    if (inLine >=noRequired){
      gameState = GameState.WON
      return(gameState)
    }

    //no winner or loser
    if (availableActions.isEmpty) {
      gameState = GameState.EVEN
      return(gameState)
    }

    else {
      gameState = GameState.ONGOING
      return(gameState)
    }
  }
}

trait UpdateTicTacToe extends Update[Location]{
  override var availableActions = (for (i <- 0 to 2; j <- 0 to 2) yield (Action(Location(i, j)))).toList
  //override var availableActions2 = List(Action(Location(20, 20))) //dummy list
  override def update(action: Action[Location], f1: Array[Array[Char]],
                      f2: Array[Array[Char]], p: Player, s:State): State = {
    if (availableActions.contains(action)) {
      val r = action.data.row
      val c = action.data.col
      f1(r)(c) = p.playerLabel
      availableActions = availableActions.filter(_ != action)
      s.gameState = checkWin(r, c, f1)

      if (s.gameState.equals(GameState.ONGOING)){
        p.switch()
      }
      else if (s.gameState.equals(GameState.WON)){
        Logger.debug(s"Game over. Player ${p.playerLabel} has won." )
      }
      else{
        Logger.debug(s"Game over. Draw!")
      }
      return s
    }

    else{
      Logger.debug(s"Field not available, please try again!")
      return s
    }
  }
  def checkWin = super.checkWin(_:Int, _: Int, 2, 2, 3, _:Array[Array[Char]])
}


trait UpdateFourWins extends Update[Column]{
  private var curRow = Array.fill(7)(0) //per column, remember last free row
  override var availableActions = (for (c <- 0 to 6) yield (Action(Column(c)))).toList
  //override var availableActions2 = List(Action(Column(20))) //dummy list
  override def update(action: Action[Column], f1: Array[Array[Char]],
                      f2: Array[Array[Char]],  p: Player, s: State): State = {
    if (availableActions.contains(action)) {
      val c = action.data.col
      f1(curRow(c))(c) = p.playerLabel
      if (curRow(c) >= 5){
        availableActions = availableActions.filter(_ != action) //if column is full, remove from available actions
      }

      s.gameState = checkWin(curRow(c), c, f1)
      curRow(c) += 1
      if (s.gameState.equals(GameState.ONGOING)){
        p.switch()
      }
      else if (s.gameState.equals(GameState.WON)){
        Logger.debug(s"Game over. Player ${p.playerLabel} has won." )
      }
      else{
        Logger.debug(s"Game over. Draw!")
      }

      return s
    }
    else{
      Logger.debug(s"Column already full, please try again!")
      return s
    }
  }
  def checkWin = super.checkWin(_:Int, _: Int, 5, 6, 4, _:Array[Array[Char]])
}



trait UpdateBattleship extends Update[Location]{
  override var availableActions = (for (i <- 0 to 9; j <- 0 to 9) yield (Action(Location(i, j)))).toList
  var availableActions2 = (for (i <- 0 to 9; j <- 0 to 9) yield (Action(Location(i, j)))).toList

  override def update(action: Action[Location], f1: Array[Array[Char]],
                      f2: Array[Array[Char]], p: Player, s:State): State = {
    if (f2.isEmpty){
      throw new IllegalArgumentException()
    }

    else {
      val r = action.data.row
      val c = action.data.col

      var h: Char = ' '

      if (p.playerLabel.equals('X') && availableActions.contains(action)) {
        h = hit(f2, r, c)
        s.gameState = checkWin(f2)
        availableActions = availableActions.filter(_ != action)
      }
      else if (p.playerLabel.equals('O') && availableActions2.contains(action)){
        h = hit(f1, r, c)
        s.gameState = checkWin(f1)
        availableActions2 = availableActions2.filter(_ != action)
      }
      else {
        Logger.debug(s"You already shot this field, please try again!")
        return s
      }

      nextTurn(h, p, s.gameState)
      return s
    }
  }

  def hit(field: Array[Array[Char]], r: Int, c: Int): Char = {
    val hit = field(r)(c) match{
      case '_' => 'W' //water
      case 'B' => 'H' //hit
    }
    field(r)(c) = hit
    if (hit.equals('W')){
    }
    return hit
  }

  def checkWin(field: Array[Array[Char]]): GameState.Value = {
    val state:GameState.Value = field.find(_.contains('B')) match {
      case Some(x) => GameState.ONGOING
      case None => GameState.WON
    }
    return state
  }

  def nextTurn(hit: Char, p: Player, gameState: GameState.Value): Unit = {
    if (hit.equals('W')) {
      Logger.debug(s"Player ${p.playerLabel} has hit the water.")
      p.switch()
    }
    else if (hit.equals('H') && gameState.equals(GameState.WON)) {
      Logger.debug(s"Game over. Player ${p.playerLabel} has won.")
    }
    else if (hit.equals('H') && gameState.equals(GameState.ONGOING)) {
      Logger.debug(s"Player ${p.playerLabel} has hit a ship. He may shoot again.")
    }
    else {
      Logger.debug(s"Sorry, something went wrong here! Hit: $hit gameState: $gameState")
    }
  }
}


case class Action[T](data: T) //für Tic Tac Toe & Battleship: Location, für 4 gewinnt: Spalte

class State{
  var gameState: GameState.Value  = GameState.ONGOING //won, lost, even, ongoing
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

case class TicTacToeGame() extends GameWrapper {
  val game = new Game[Location] with UpdateTicTacToe with FieldTicTacToe {}

  def getAvailableActionWrappers(): List[ActionWrapper] =
    game.getAvailableActions.map(a => ActionWrapper(List(a.data.r,a.data.c))).toList

  def updateStateWrapper(action: ActionWrapper): StateWrapper = {
    val unwrappedAction = Action(Location(action.data.toList(0),action.data.toList(1)))
    game.updateGameState(unwrappedAction)
    currentState
  }

  def currentState : StateWrapper = {
    StateWrapper(game.state.gameState.toString, game.player.playerLabel,
      game.field.toList.map(a => a.toList).toList)
  }
}


case class FourWinsGame() extends GameWrapper {
  val game = new Game[Column] with UpdateFourWins with FieldFourWins {}

  def getAvailableActionWrappers(): List[ActionWrapper] =
    game.getAvailableActions.map(a => ActionWrapper(List(a.data.c))).toList

  def updateStateWrapper(action: ActionWrapper): StateWrapper = {
    val unwrappedAction = Action(Column(action.data.toList(0)))
    game.updateGameState(unwrappedAction)
    currentState
  }

  def currentState : StateWrapper = {
    StateWrapper(game.state.gameState.toString, game.player.playerLabel,
      game.field.toList.map(a => a.toList).toList)
  }
}

case class BattleshipGame() extends GameWrapper {
  val game = new Game[Location] with UpdateBattleship with FieldBattleship {}

  def getAvailableActionWrappers(): List[ActionWrapper] =
    game.getAvailableActions.map(a => ActionWrapper(List(a.data.r,a.data.c))).toList

  def updateStateWrapper(action: ActionWrapper): StateWrapper = {
    val unwrappedAction = Action(Location(action.data.toList(0),action.data.toList(1)))
    game.updateGameState(unwrappedAction)
    currentState
  }

  def currentState : StateWrapper = {
    if (!game.field2.isEmpty){
      StateWrapper(game.state.gameState.toString, game.player.playerLabel,
        game.field.toList.map(a => a.toList).toList ++ game.field2.toList.map(a => a.toList).toList)
    }
    else{
      throw new IllegalArgumentException()
    }
  }
}
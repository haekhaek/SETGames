package model

trait Game[T]{
  def update(action: Action[T]): Status = {
    if (availableActions().isEmpty){
      ???
    }
  ???
  }
  def availableActions(): List[Action[T]]
  val field: Array[Array[Char]]
  var player: Char
}

case class Action[T](data: T) //für Tic Tac Toe: Location, für 4 gewinnt: Spalte

class Status{
  val playerLabel: Char = ' '
}

class TicTacToe extends Game[(Int, Int)]{
  override def update(action: Action[(Int, Int)]): Status = {
    ???
  }
  override def availableActions() = {
    ???
  }
  override val field = Array.ofDim(3, 3)
  override var player: Char = _
}




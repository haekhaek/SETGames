package model

trait Game[T]{
  def update(action: Action[T]): Status
  def availableActions(): List[Action[T]]
}

case class Action[T](data: T)
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
  val field: Array[Array[Char]]  = Array.ofDim(3, 3)
}




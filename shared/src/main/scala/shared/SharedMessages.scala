package shared

object SharedMessages {
  def itWorks = "It works!"
}

case class ClientMessage(messageType : Int, message : String)

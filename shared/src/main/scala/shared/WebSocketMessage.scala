package shared

import prickle.Unpickle
import prickle.Pickle
import scala.util.Success
import scala.util.Failure

case class WebSocketMessage(
    val messageType : Int = 0,
    val sender : String,
    val receiver : String,
    val data : String)
    
object WebSocketMessage {

  def parse(message : String) : WebSocketMessage = Unpickle[WebSocketMessage].fromString(message) match {
    case Success(s) => s
    case Failure(e) => throw new IllegalArgumentException(e.getMessage)
  }
  
  def stringify(message : WebSocketMessage) = Pickle.intoString(message)

}
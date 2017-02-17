package shared

import prickle.Unpickle
import prickle.Pickle
import prickle.CompositePickler
import scala.util.Success
import scala.util.Failure

sealed trait WebSocketMessage {
    val sender : String
    val receiver : String
}
    
object WebSocketMessage extends Enumeration {

  implicit val messagePickler = CompositePickler[WebSocketMessage]
    .concreteType[UserUpdateMessage]
    .concreteType[ErrorMessage]
    .concreteType[NotificationMessage]
    .concreteType[ChallengeDeclineMessage]
    .concreteType[ChallengeAcceptMessage]
    .concreteType[ChallengeMessage]
    .concreteType[GameUpdateMessage]
    .concreteType[GameSurrenderMessage]
    .concreteType[GameActionMessage]

  def parse(message : String) : WebSocketMessage = Unpickle[WebSocketMessage].fromString(message) match {
    case Success(s) => s
    case Failure(e) => throw new IllegalArgumentException(e.getMessage)
  }
  
  def stringify(message : WebSocketMessage) = Pickle.intoString(message)

}

case class GameActionMessage(override val sender : String,
                             override val receiver : String,
                             val action : ActionWrapper) extends WebSocketMessage

case class GameUpdateMessage(override val sender : String,
                             override val receiver : String,
                             val state : StateWrapper) extends WebSocketMessage

case class GameSurrenderMessage(override val sender : String,
                                override val receiver : String) extends WebSocketMessage

case class ChallengeMessage(override val sender : String,
                            override val receiver : String) extends WebSocketMessage

case class ChallengeAcceptMessage(override val sender : String,
                                  override val receiver : String) extends WebSocketMessage

case class ChallengeDeclineMessage(override val sender : String,
                                   override val receiver : String) extends WebSocketMessage

case class NotificationMessage(override val sender : String,
                               override val receiver : String,
                               val content : String) extends WebSocketMessage

case class ErrorMessage(override val sender : String,
                        override val receiver : String,
                        val content : String) extends WebSocketMessage

case class UserUpdateMessage(override val sender : String,
                        override val receiver : String,
                        val userList : Iterable[String]) extends WebSocketMessage

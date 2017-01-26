package example

import shared.WebSocketMessage
import shared.WebSocketMessage._
import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom
import org.scalajs.dom.html
import scalatags.JsDom.all._
import scala.scalajs.js.timers._
import scala.util.Success
import scala.util.Failure
import prickle.Unpickle

class WebSocketMessageHandler(
    val userName : String,
    val connection : dom.WebSocket) {

    def handle(message : WebSocketMessage) = {
        if(message.messageType == NOTIFICATION.id) {
            DomUtil.displayMessage(DomMessage(
            s"${message.sender}: ",
            s"${message.data}",
            "success"))
        }
    }
}

trait ChallengeHandler extends WebSocketMessageHandler {
    override def handle(message : WebSocketMessage) = {
        if(message.messageType == CHALLENGE.id) {
            DomUtil.displayChallenge(connection, message)
        }
        super.handle(message)
    }
}

trait ChallengeAcceptHandler extends WebSocketMessageHandler {
    override def handle(message : WebSocketMessage) = {
        if(message.messageType == CHALLENGE_ACCEPT.id) {
            DomUtil.displayMessage(DomMessage(
                s"${message.sender} ",
                "has accepted your challenge.",
                "warning"))
        }
        super.handle(message)
    }
}

trait ChallengeDeclinedHandler extends WebSocketMessageHandler {
    override def handle(message : WebSocketMessage) = {
        if(message.messageType == CHALLENGE_DECLINE.id) {
            DomUtil.displayMessage(DomMessage(
                s"${message.sender} ",
                "has declined your challenge.",
                "warning"))
        }
        super.handle(message)
    }
}

trait UserUpdateHandler extends WebSocketMessageHandler {
    override def handle(message : WebSocketMessage) = {
        if(message.messageType == USER_UPDATE.id) {
            Unpickle[Iterable[String]].fromString(message.data) match {
                case Success(activeMembers) =>
                    DomUtil.updateMemberList(connection, userName, activeMembers)
                case Failure(e) =>
                    throw new IllegalArgumentException(e.getMessage)
            }
        }
        super.handle(message)
    }
}
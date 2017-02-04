package example

import shared.{StateWrapper, ActionWrapper}
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
import prickle.Pickle

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
            DomUtil.deactivateChallengeButtons(userName, connection)
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
            DomUtil.deactivateChallengeButtons(userName, connection)
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
            DomUtil.activateChallengeButtons(userName, connection)
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

trait ErrorHandler extends WebSocketMessageHandler {
    override def handle(message : WebSocketMessage) = {
        if(message.messageType == GAME_ACTION.id) {
            DomUtil.displayMessage(DomMessage(
            "Error: ",
            s"${message.data}",
            "error"))
        }
        super.handle(message)
    }
}

// 0. Update field data
// 1. Check if playerLabel == myLabel?
// 2. If true => activate field
// 3. Else => deactivate field
trait StupidButtonUpdateHandler extends WebSocketMessageHandler {
    val button : html.Button
    override def handle(message : WebSocketMessage) = {
        if(message.messageType == GAME_UPDATE.id) {
            Unpickle[StateWrapper].fromString(message.data) match {
                case Success(state) =>
                    if(s"${state.playerLabel}".equals(DomUtil.currentPlayerLabel)) {
                        button.classList.remove("disabled")
                        button.disabled = false
                        button.onclick = { (e: dom.Event) =>
                            val action = ActionWrapper(List(42))
                            connection.send(stringify(WebSocketMessage(
                                GAME_ACTION.id, userName, message.sender,
                                Pickle.intoString(action))))
                            button.classList.add("disabled")
                            button.disabled = true
                        }
                    } else {
                        button.classList.add("disabled")
                        button.disabled = true
                    }
                case Failure(e) =>
                    throw new IllegalArgumentException(e.getMessage)
            }
        }
        super.handle(message)
    }
}

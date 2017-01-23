package example

import shared.WebSocketMessage
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

@JSExport
object OnlineMemberList {
  
  @JSExport
  def shoutMessageLogic(
        clientAddress : String,
        send : html.Button,
        message : html.Input,
        connection : dom.WebSocket) {
    val messages = dom.document.getElementById("notifications")
    val memberList = dom.document.getElementById("memberList")
    send.disabled = true
    val sendFunc = () => {
        val text = message.value
        message.value = ""
        WebSocketUtil.send(connection, WebSocketMessage(WebSocketMessage.NOTIFICATION.id, clientAddress, "all", text))
    }
    
    val onConnectionOpenedHandler = { (e: dom.Event) =>
        send.disabled = false
        send.onclick = { (e: dom.Event) =>
            sendFunc()
        }
        val onpress = (e: dom.KeyboardEvent) => {
          if (e.keyCode == KeyCode.enter){
            sendFunc()
          }
        }
        message.onkeypress = onpress
    }
    
    val incomingMessageHandler = (e: dom.MessageEvent) => {
            val message = WebSocketMessage.parse(e.data.toString)
            if(message.messageType == WebSocketMessage.NOTIFICATION.id) {
                messages.appendChild(div(
                    cls:="alert alert-success notify",
                    a(href:="#", cls:="close", data("dismiss"):="alert", aria.label:="close", "×"),
                    strong(s"${message.sender}: "),
                    Some(s"${message.data}")
                ).render)
            } else if(message.messageType == WebSocketMessage.USER_UPDATE.id) {
                Unpickle[Iterable[String]].fromString(message.data) match {
                    case Success(activeMembers) => {
                        memberList.innerHTML = ""
                        for {
                            member <- activeMembers
                        } yield {
                            if(!member.equals(clientAddress)) {
                                val clickListener = { (e: dom.Event) =>
                                    WebSocketUtil.send(connection, WebSocketMessage(WebSocketMessage.CHALLENGE.id, clientAddress, member, ""))
                                }
                                memberList.appendChild(li(div(Some(member),
                                    button("Challenge!", onclick:=clickListener))).render)
                            }
                        }
                    }
                    case Failure(e) => throw new IllegalArgumentException(e.getMessage)
                }
            } else if(message.messageType == WebSocketMessage.CHALLENGE.id) {
                val challengeDiv = div(
                    cls:="alert alert-warning notify",
                    strong(s"${message.sender} "),
                    Some("wants to play against you. ")
                ).render
                challengeDiv.appendChild(a("[Accept]", onclick:={(e: dom.Event) => {
                        challengeDiv.style.display = "none"
                        WebSocketUtil.send(
                            connection,
                            WebSocketMessage(WebSocketMessage.CHALLENGE_ACCEPT.id, message.receiver, message.sender, ""))
                    }}).render)
                challengeDiv.appendChild(a("[Decline]", onclick:={(e: dom.Event) => {
                        challengeDiv.style.display = "none"
                        WebSocketUtil.send(
                            connection,
                            WebSocketMessage(WebSocketMessage.CHALLENGE_DECLINE.id, message.receiver, message.sender, ""))
                    }}).render)
                messages.appendChild(challengeDiv)
            } else if(message.messageType == WebSocketMessage.CHALLENGE_ACCEPT.id) {
                messages.appendChild(div(
                    cls:="alert alert-warning notify",
                    a(href:="#", cls:="close", data("dismiss"):="alert", aria.label:="close", "×"),
                    strong(s"${message.sender} "),
                    Some("has accepted your challenge.")
                ).render)
            } else if(message.messageType == WebSocketMessage.CHALLENGE_DECLINE.id) {
                messages.appendChild(div(
                    cls:="alert alert-warning notify",
                    a(href:="#", cls:="close", data("dismiss"):="alert", aria.label:="close", "×"),
                    strong(s"${message.sender} "),
                    Some("has declined your challenge.")
                ).render)
            }
            ()
          }
    WebSocketUtil.setup(connection, onConnectionOpenedHandler, incomingMessageHandler)
  }
}
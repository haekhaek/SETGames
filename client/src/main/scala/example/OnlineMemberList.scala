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
        WebSocketUtil.send(connection,
            WebSocketMessage(NOTIFICATION.id, clientAddress, "all", text))
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
            if(message.messageType == NOTIFICATION.id) {
                displayMessage(s"${message.sender}: ", s"${message.data}", "success")
            } else if(message.messageType == USER_UPDATE.id) {
                Unpickle[Iterable[String]].fromString(message.data) match {
                    case Success(activeMembers) => {
                        memberList.innerHTML = ""
                        for {
                            member <- activeMembers
                        } yield {
                            if(!member.equals(clientAddress)) {
                                val clickListener = { (e: dom.Event) =>
                                    WebSocketUtil.send(connection,
                                    WebSocketMessage(CHALLENGE.id, clientAddress, member, ""))
                                }
                                memberList.appendChild(li(div(Some(member),
                                    button("Challenge!", onclick:=clickListener))).render)
                            }
                        }
                    }
                    case Failure(e) => throw new IllegalArgumentException(e.getMessage)
                }
            } else if(message.messageType == CHALLENGE.id) {
                val challengeDiv = alertDiv(s"${message.sender} ", "wants to play against you. ", "warning")
                challengeDiv.appendChild(challengeOption("[Accept]", CHALLENGE_ACCEPT.id,
                    challengeDiv, connection, message))
                challengeDiv.appendChild(challengeOption("[Decline]", CHALLENGE_DECLINE.id,
                    challengeDiv, connection, message))
                messages.appendChild(challengeDiv)
            } else if(message.messageType == CHALLENGE_ACCEPT.id) {
                displayMessage(s"${message.sender} ", "has accepted your challenge.", "warning")
            } else if(message.messageType == CHALLENGE_DECLINE.id) {
                displayMessage(s"${message.sender} ", "has declined your challenge.", "warning")
            }
            ()
          }
    WebSocketUtil.setup(connection, onConnectionOpenedHandler, incomingMessageHandler)
  }
  
  def challengeOption(caption : String,
                    messageType : Int,
                    challengeDiv : dom.html.Div,
                    connection : dom.WebSocket,
                    message : WebSocketMessage) =
    a(caption, onclick:={(e: dom.Event) => {
        challengeDiv.style.display = "none"
        WebSocketUtil.send(
            connection,
            WebSocketMessage(messageType, message.receiver, message.sender, ""))
    }}).render
  
  def displayMessage(title: String, message : String, alertType : String) {
    val messages = dom.document.getElementById("notifications")
    messages.appendChild(alertDiv(title, message, alertType))
  }
  
  def alertDiv(title: String, message : String, alertType : String) = div(
        cls:=s"alert alert-${alertType} notify",
        a(href:="#", cls:="close", data("dismiss"):="alert", aria.label:="close", "×"),
        strong(s"${title} "),
        Some(message)
    ).render
}
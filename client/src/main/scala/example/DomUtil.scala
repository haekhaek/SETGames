package example

import shared.WebSocketMessage
import shared.WebSocketMessage._
import scala.scalajs.js
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom
import org.scalajs.dom.html
import scalatags.JsDom.all._
import scala.scalajs.js.timers._

case class DomMessage(title: String, message : String, alertType : String)

object DomUtil {

    val notificationId = "notifications"

    def setupShoutMessenger(
        userName : String,
        send : html.Button,
        message : html.Input,
        connection : dom.WebSocket) : dom.Event => Unit = {
        send.disabled = true
        val sendFunc = () => {
            val text = message.value
            message.value = ""
            WebSocketUtil.send(connection,
                WebSocketMessage(NOTIFICATION.id, userName, "all", text))
        }
        return { (e: dom.Event) =>
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
    }
    
    def updateMemberList(
        connection : dom.WebSocket,
        userName : String,
        activeMembers : Iterable[String]) {
        val memberList = dom.document.getElementById("memberList")
        memberList.innerHTML = ""
        for {
            member <- activeMembers
        } yield {
            if(!member.equals(userName)) {
                val clickListener = { (e: dom.Event) =>
                    WebSocketUtil.send(connection,
                    WebSocketMessage(CHALLENGE.id, userName, member, ""))
                }
                memberList.appendChild(li(div(Some(member),
                button("Challenge!", onclick:=clickListener))).render)
            }
        }
    }

    def displayChallenge(
        connection : dom.WebSocket,
        message : WebSocketMessage) {
        val messages = dom.document.getElementById(notificationId)
        val challengeDiv = alertDiv(DomMessage(
            s"${message.sender} ",
            "wants to play against you. ",
            "warning"))
        challengeDiv.appendChild(challengeOption("[Accept]", CHALLENGE_ACCEPT.id,
                    challengeDiv, connection, message))
        challengeDiv.appendChild(challengeOption("[Decline]", CHALLENGE_DECLINE.id,
        challengeDiv, connection, message))
        messages.appendChild(challengeDiv)
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

    def displayMessage(m : DomMessage) {
        val messages = dom.document.getElementById(notificationId)
        messages.appendChild(alertDiv(m))
    }
  
    def alertDiv(m : DomMessage) = div(
            cls:=s"alert alert-${m.alertType} notify",
            a(href:="#", cls:="close", data("dismiss"):="alert", aria.label:="close", "×"),
            strong(s"${m.title} "),
            Some(m.message)
        ).render

}

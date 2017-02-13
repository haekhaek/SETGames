package js

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
    
    var cachedActiveMembers : Iterable[String] = List()
    
    var playingGame : Boolean = false

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
        cachedActiveMembers = activeMembers
        if(!playingGame) {
            activateChallengeButtons(userName, connection)
        }
    }
    
    def deactivateChallengeButtons(
        userName : String,
        connection : dom.WebSocket) {
        val memberList = dom.document.getElementById("memberList")
        memberList.innerHTML = ""
        for {
            member <- cachedActiveMembers
        } yield {
            if(!member.equals(userName)) {
                val clickListener = { (e: dom.Event) =>
                    WebSocketUtil.send(connection,
                    WebSocketMessage(CHALLENGE.id, userName, member, ""))
                }
                memberList.appendChild(li(div(Some(member), cls:="buttonChallenge")).render)
            }
        }
    }
    
    def activateChallengeButtons(
        userName : String,
        connection : dom.WebSocket) {
        val memberList = dom.document.getElementById("memberList")
        memberList.innerHTML = ""
        for {
            member <- cachedActiveMembers
        } yield {
            if(!member.equals(userName)) {
                val clickListener = { (e: dom.Event) =>
                    WebSocketUtil.send(connection,
                    WebSocketMessage(CHALLENGE.id, userName, member, ""))
                    deactivateChallengeButtons(userName, connection)
                    clearMessages
                    displayMessage(DomMessage("You ",
                    s"are challenging ${member}. ",
                    "warning"))
                    setCurrentPlayerLabel("X")
                }
                memberList.appendChild(li(cls:="list-group-item",div(cls:="row",
                    div(cls:="col-sm-8", style:="font-size: 16px; position: relative; top: 50%; transform: translateY(30%); ", Some(member)),
                    div(cls:="col-sm-4",button("Challenge!",
                    onclick:=clickListener,
                    cls:="btn btn-warning buttonChallenge"))
                    )
                ).render)
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
            challengeDiv, connection, message,
            s => {
                playingGame = true
                deactivateChallengeButtons(s, connection)
                clearMessages
                displayMessage(DomMessage("You ",
                s"accepted ${message.sender}'s challenge. ",
                "warning"))
                setCurrentPlayerLabel("O")
            }))
        challengeDiv.appendChild(challengeOption("[Decline]", CHALLENGE_DECLINE.id,
            challengeDiv, connection, message,
            activateChallengeButtons(_, connection)))
        messages.appendChild(challengeDiv)
    }
    
    def challengeOption(caption : String,
                    messageType : Int,
                    challengeDiv : dom.html.Div,
                    connection : dom.WebSocket,
                    message : WebSocketMessage,
                    func : String => Unit) =
        a(caption, onclick:={(e: dom.Event) => {
            challengeDiv.style.display = "none"
            func(message.receiver)
            WebSocketUtil.send(
                connection,
                WebSocketMessage(messageType, message.receiver, message.sender, ""))
        }}).render

    def displayMessage(m : DomMessage) {
        val messages = dom.document.getElementById(notificationId)
        messages.appendChild(alertDiv(m))
    }
    
    def clearMessages = {
        val messages = dom.document.getElementById(notificationId)
        messages.innerHTML = ""
    }
  
    def alertDiv(m : DomMessage) = div(
            cls:=s"alert alert-${m.alertType} notify",
            a(href:="#", cls:="close", data("dismiss"):="alert", aria.label:="close", "×"),
            strong(s"${m.title} "),
            Some(m.message)
        ).render
    
    def setPlayingGame(isPlaying : Boolean) = {
        playingGame = isPlaying
    }

    def currentPlayerLabel : String = dom.
        document.getElementById("currentPlayerLabel").innerHTML
    
    def setCurrentPlayerLabel(label : String) = {
        dom.document.getElementById("currentPlayerLabel").innerHTML = label
    }
}

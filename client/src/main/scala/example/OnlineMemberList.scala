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

@JSExport
object OnlineMemberList {
  
  @JSExport
  def shoutMessageLogic(
        userName : String,
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
            WebSocketMessage(NOTIFICATION.id, userName, "all", text))
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
    WebSocketUtil.setup(
        connection,
        onConnectionOpenedHandler,
        new WebSocketMessageHandler(userName, connection)
            with ChallengeHandler
            with ChallengeAcceptHandler
            with ChallengeDeclinedHandler
            with UserUpdateHandler)
  }
}
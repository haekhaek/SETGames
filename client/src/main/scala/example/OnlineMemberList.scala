package example

import shared.WebSocketMessage
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
        clientAddress : String,
        send : html.Button,
        message : html.Input,
        connection : dom.WebSocket) {
    val messages = dom.document.getElementById("notifications")
    send.disabled = true
    val sendFunc = () => {
        val text = message.value
        message.value = ""
        WebSocketUtil.send(connection, WebSocketMessage(MessageType.chat, clientAddress, "all", text))
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
            if(message.receiver.equals("all")) {
                /*setTimeout(5000) {() =>
                    messages.appendChild(div(
                        cls:="alert alert-success notify",
                        a(href:="#", cls:="close", data("dismiss"):="alert", aria.label:="close", "×"),
                        strong(s"${message.sender}: "),
                        Some(s"${message.data}")
                    ).render)
                }*/
                messages.appendChild(div(
                        cls:="alert alert-success notify",
                        a(href:="#", cls:="close", data("dismiss"):="alert", aria.label:="close", "×"),
                        strong(s"${message.sender}: "),
                        Some(s"${message.data}")
                    ).render)
            }
            ()
          }
    WebSocketUtil.setup(connection, onConnectionOpenedHandler, incomingMessageHandler)
  }
}
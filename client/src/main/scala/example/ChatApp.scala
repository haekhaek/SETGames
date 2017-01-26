package example

import shared.WebSocketMessage
import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom
import org.scalajs.dom.html
import scalatags.JsDom.all._

@JSExport
object ChatApp {
  
  @JSExport
  def chatLogic(clientAddress : String,
        send : html.Button,
        message : html.Input,
        receiver : html.Input,
        connection : dom.WebSocket) {
    val messages = dom.document.getElementById("messages")
    send.disabled = true
    val sendFunc = () => {
        val text = message.value
        message.value = ""
        val receiverAddress = if(receiver.value == null || receiver.value.isEmpty) {"all"} else {receiver.value}
        WebSocketUtil.send(connection, WebSocketMessage(MessageType.chat, clientAddress, receiverAddress, text))
        messages.appendChild(li(style:="font-size: 1.5em", s"Me: ${text}").render)
    }
    
    val onConnectionOpenedHandler = { (e: dom.Event) =>
        send.disabled = false
        messages.appendChild(li(
            cls:="bg-info",
            style:="font-size: 1.5em",
            "Connected").render)
        send.onclick = { (e: dom.Event) =>
            sendFunc()
        }
        val onpress = (e: dom.KeyboardEvent) => {
          if (e.keyCode == KeyCode.enter){
            sendFunc()
          }
        }
        message.onkeypress = onpress
        receiver.onkeypress = onpress
    }
    
    val incomingMessageHandler = (e: dom.MessageEvent) => {
            val message = WebSocketMessage.parse(e.data.toString)
            if(message.messageType == MessageType.chat && !message.sender.equals(clientAddress)) {
                messages.appendChild(li(style:="font-size: 1.5em", s"${message.sender}: ${message.data}").render)
            }
            ()
          }
    //WebSocketUtil.setup(connection, onConnectionOpenedHandler, incomingMessageHandler)
  }
}
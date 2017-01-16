package example

import shared.WebSocketMessage
import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom
import org.scalajs.dom.html

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
        messages.innerHTML += s"<li style='font-size: 1.5em'>Me: ${text}</li>"
    }
    
    val onConnectionOpenedHandler = { (e: dom.Event) =>
        send.disabled = false
        messages.innerHTML = s"<li class='bg-info' style='font-size: 1.5em'>Connected</li>${messages.innerHTML}"
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
                messages.innerHTML += s"<li style='font-size: 1.5em'>${message.sender}: ${message.data}</li>"
            }
          }
    WebSocketUtil.setup(connection, onConnectionOpenedHandler, incomingMessageHandler)
  }
}
package example

import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom
import org.scalajs.dom.html
import shared.SharedMessages

@JSExport
object ChatApp {
  @JSExport
  def main(): Unit = {
    dom.document.getElementById("scalajsShoutOut").textContent = SharedMessages.itWorks
  }
  
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
        WebSocketUtil.send(connection, WebSocketMessage(0, clientAddress, receiverAddress, text))
    }
    
    val onConnectionOpenedHandler = { (e: dom.Event) =>
        send.disabled = false
        messages.innerHTML = s"<li class='bg-info' style='font-size: 1.5em'>Connected</li>${messages.innerHTML}"
        send.onclick = { (e: dom.Event) =>
            sendFunc()
        }
        message.onkeypress = (e: dom.KeyboardEvent) => {
          if (e.keyCode == KeyCode.enter){
            sendFunc()
          }
        }
    }
    
    val incomingMessageHandler = (e: dom.MessageEvent) =>
          messages.innerHTML += s"<li style='font-size: 1.5em'>${e.data}</li>"
    WebSocketUtil.setup(connection, onConnectionOpenedHandler, incomingMessageHandler)
  }
}
package js

import shared.WebSocketMessage
import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom
import org.scalajs.dom.html
import shared.SharedMessages

object MessageType {
    val chat : Int = 0
}

object WebSocketUtil {

  def send(socket : dom.WebSocket,
            socketMessage : WebSocketMessage) {
    socket.send(WebSocketMessage.stringify(socketMessage))
  }

  def setup(socket : dom.WebSocket,
            onConnectionOpenedHandler : dom.Event => Unit,
            incomingMessageHandler : WebSocketMessageHandler) {
    socket.onopen = onConnectionOpenedHandler
    socket.onmessage = (e: dom.MessageEvent) => {
        val message = WebSocketMessage.parse(e.data.toString)
        incomingMessageHandler.handle(message)
    }
  }

}
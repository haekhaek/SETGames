package example

import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom
import org.scalajs.dom.html
import shared.SharedMessages

case class WebSocketMessage(
    val messageType : Int = 0,
    val sender : String,
    val receiver : String,
    val message : String)

object WebSocketUtil {

  def send(socket : dom.WebSocket,
            socketMessage : WebSocketMessage) {
    socket.send("{\"messageType\" : " + socketMessage.messageType +
                        ", \"receiver\":\"" + socketMessage.receiver +
                        "\", \"message\": \"" + socketMessage.message +
                        "\", \"sender\": \""+ socketMessage.sender +"\"}")
  }

  def setup(socket : dom.WebSocket,
            onConnectionOpenedHandler : dom.Event => Unit,
            incomingMessageHandler : dom.MessageEvent => Unit) {
    socket.onopen = onConnectionOpenedHandler
    socket.onmessage = incomingMessageHandler
  }

}
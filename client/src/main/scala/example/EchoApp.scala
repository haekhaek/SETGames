package example

import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom
import org.scalajs.dom.html
import shared.SharedMessages
import shared.ClientMessage

@JSExport
object EchoApp {
  @JSExport
  def echoLogic(connection : dom.WebSocket) {
    connection.onmessage = {
        (e: dom.MessageEvent) =>
          println(s"${e.data}")
      }
    connection.onopen = { (e: dom.Event) =>
        connection.send("{\"messageType\" : 1, \"receiver\":\"all\", \"message\": \"Woohoo!\"}")
    }
  }
}
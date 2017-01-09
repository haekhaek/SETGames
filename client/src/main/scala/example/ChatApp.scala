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
    println("Hello world!")
  }
  
  @JSExport
  def chatLogic(clientAddress : String, send : html.Button,
    message : html.Input, connection : dom.WebSocket) {
    val messages = dom.document.getElementById("messages")
    send.disabled = true
    val sendFunc = () => {
        val text = message.value
        message.value = ""
        connection.send(s"$clientAddress: $text");
    }
    
    connection.onopen = { (e: dom.Event) =>
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
    
    connection.onmessage = {
        (e: dom.MessageEvent) =>
          messages.innerHTML += s"<li style='font-size: 1.5em'>${e.data}</li>"
      }
  }
}
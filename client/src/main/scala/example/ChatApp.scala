package example

import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom
import shared.SharedMessages

@JSExport
object ChatApp {
  @JSExport
  def main(): Unit = {
    dom.document.getElementById("scalajsShoutOut").textContent = SharedMessages.itWorks
    println("Hello world!")
  }
  
  @JSExport
  def chatLogic(url : String, clientAddress : String) {
    println(s"$url <-> $clientAddress")
    val messages = dom.document.getElementById("messages")
    val send = dom.document.getElementById("send")
    val message = dom.document.getElementById("message")
    val connection : dom.WebSocket = dom.WebSocket(url)
    
    val sendFunc = () => {
                    val text = message.textContent
                    message.textContent = ""
                    connection.send(s"$clientAddress: $text");
    }
    
    connection.onopen = { (e: dom.Event) =>
        messages.textContent = s"<li class='bg-info' style='font-size: 1.5em'>Connected</li>${messages.textContent}"
        send.onclick = { (e: dom.Event) =>
          sendFunc()
        }
    }
    
    connection.onmessage = {
        (e: dom.MessageEvent) =>
          messages.textContent += s"<li style='font-size: 1.5em'>${e.data}</li>"
      }
  }
}
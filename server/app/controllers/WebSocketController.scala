package controllers

import javax.inject._

import shared.WebSocketMessage
import akka.actor._
import akka.stream.Materializer
import play.api.mvc._
import play.api.libs.streams.ActorFlow
import scala.concurrent.ExecutionContext

@Singleton
class WebSocketController @Inject()(implicit actorSystem: ActorSystem,
                               mat: Materializer,
                               executionContext: ExecutionContext) extends Controller {

  def websocket = WebSocket.accept[String, String] { request =>
    ActorFlow.actorRef(out => {
        UserTracker.update(request.remoteAddress, out)
        ClientActor.props(out)
    })
  }

}

object ClientActor {
    def props(out : ActorRef) = Props(new ClientActor(out))
}

class ClientActor(out : ActorRef) extends Actor {
    def receive = {
        case message : String => forward(WebSocketMessage.parse(message), message)
    }
    
    def forward(socketMessage : WebSocketMessage, message : String) : Unit = socketMessage.receiver match {
        case "all" => UserTracker.broadcast(message)
        case m => UserTracker.sendTo(m, message)
    }

}
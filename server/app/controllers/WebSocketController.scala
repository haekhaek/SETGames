package controllers

import javax.inject._

import akka.actor._
import akka.stream.Materializer
import play.api.mvc._
import play.api.libs.streams.ActorFlow
import play.api.libs.json._
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
        case message : String => toMessage(Json.parse(message), message)
    }
    
    def toMessage(json : JsValue, message : String) : Unit = (json \ "receiver").as[JsString] match {
        case JsString(x) => broadcastOrP2P(x, message)
        case _ => out ! "Error"
    }
    
    def broadcastOrP2P(x : String, message : String) = x match {
        case "all" => UserTracker.broadcast(message)
        case m => UserTracker.sendTo(m, message)
    }
}
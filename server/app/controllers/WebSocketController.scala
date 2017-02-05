package controllers

import javax.inject._

import shared.{GameWrapper, ActionWrapper}
import shared.WebSocketMessage
import shared.WebSocketMessage._
import akka.actor._
import akka.stream.Materializer
import play.api.mvc._
import play.api.libs.streams.ActorFlow
import scala.concurrent.ExecutionContext
import prickle.Unpickle
import prickle.Pickle
import scala.util.Success
import scala.util.Failure

@Singleton
class WebSocketController @Inject()(implicit actorSystem: ActorSystem,
                               mat: Materializer,
                               executionContext: ExecutionContext) extends Controller {

  def websocket = WebSocket.accept[String, String] { request =>
    ActorFlow.actorRef(out => request.session.get("userName") match {
        case Some(username) => {
            UserTracker.update(username, out)
            ClientActor.props(out)
        }
        case None => throw new RuntimeException
    })
  }

}

object ClientActor {
    val forwarder = new PeerToPeerMessageForwarder
        with ChallengeAcceptForwarder
        with GameActionForwarder
        with GameSurrenderForwarder

    def props(out : ActorRef) = Props(new ClientActor(out))
}

class ClientActor(out : ActorRef) extends Actor {
    def receive = {
        case message : String => forward(WebSocketMessage.parse(message), message)
    }

    def forward(socketMessage : WebSocketMessage, message : String) : Unit = socketMessage.receiver match {
        case "all" => UserTracker.users.get(socketMessage.sender) match {
            case None => UserTracker.broadcast(message)
            case Some(record) => UserTracker.broadcastTo(message, record.game)
        }
        case m => for {
                    player1 <- UserTracker.users.get(socketMessage.receiver)
                    player2 <- UserTracker.users.get(socketMessage.sender)
                } yield {
                    ClientActor.forwarder.forward(socketMessage, player1, player2)
                }
        }
}
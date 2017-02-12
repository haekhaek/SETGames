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
import service.UserService

@Singleton
class WebSocketController @Inject()(implicit actorSystem: ActorSystem,
                               mat: Materializer,
                               executionContext: ExecutionContext, userService: UserService) extends Controller {

  def websocket = WebSocket.accept[String, String] { request =>
    ActorFlow.actorRef(out => request.session.get("userName") match {
        case Some(username) => {
            UserTracker.update(username, out)
            ClientActor.props(out, userService)
        }
        case None => throw new RuntimeException
    })
  }

}

object ClientActor {
    def props(out : ActorRef, userService : UserService) = Props(new ClientActor(out,userService))
}

class ClientActor(out : ActorRef, userService: UserService) extends Actor {
  val forwarder = new PeerToPeerMessageForwarder(userService)
    with ChallengeAcceptForwarder
    with GameActionForwarder
    with GameSurrenderForwarder
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
                    forwarder.forward(socketMessage, player1, player2)
                }
        }
}
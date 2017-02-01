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
  
  def gameWebsocket(game : Option[GameWrapper] = None) = WebSocket.accept[String, String] { request =>
    ActorFlow.actorRef(out => request.session.get("userName") match {
        case Some(username) => {
            UserTracker.update(username, out)
            game.map(g => UserTracker.updateGame(username, game))
            ClientActor.props(out)
        }
        case None => throw new RuntimeException
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
        case "all" => UserTracker.users.get(socketMessage.sender) match {
            case None => UserTracker.broadcast(message)
            case Some(record) => UserTracker.broadcastTo(message, record.game)
        }
        case m => {
            for {
                player1 <- UserTracker.users.get(socketMessage.receiver)
                player2 <- UserTracker.users.get(socketMessage.sender)
            } yield {
                if(socketMessage.messageType == WebSocketMessage.CHALLENGE_ACCEPT.id) {
                    player1.game = player2.game
                } else if(socketMessage.messageType == WebSocketMessage.GAME_ACTION.id) {
                    Unpickle[ActionWrapper].fromString(message) match {
                        case Success(actionWrapper) => player1.game.map(g => {
                            val state = g.updateStateWrapper(actionWrapper)
                            val stateMessage = Pickle.intoString(state)
                            UserTracker.sendTo(socketMessage.sender, stringify(WebSocketMessage(
                                GAME_UPDATE.id, socketMessage.receiver, socketMessage.sender, stateMessage)))
                            UserTracker.sendTo(socketMessage.receiver, stringify(WebSocketMessage(
                                GAME_UPDATE.id, socketMessage.sender, socketMessage.receiver, stateMessage)))
                        })
                        case Failure(e) => UserTracker.sendTo(socketMessage.receiver, stringify(WebSocketMessage(
                                ERROR.id, socketMessage.sender, socketMessage.receiver, "Illegal action!")))
                    }
                } else if(socketMessage.messageType == WebSocketMessage.GAME_SURRENDER.id) {
                    player1.game.map(g => {
                            val stateMessage = Pickle.intoString(g.currentState)
                            UserTracker.sendTo(socketMessage.sender, stringify(WebSocketMessage(
                                GAME_UPDATE.id, socketMessage.receiver, socketMessage.sender, stateMessage)))
                            UserTracker.sendTo(socketMessage.receiver, stringify(WebSocketMessage(
                                GAME_UPDATE.id, socketMessage.sender, socketMessage.receiver, stateMessage)))
                    })
                }
            }
            
            UserTracker.sendTo(m, message)
        }
    }

}
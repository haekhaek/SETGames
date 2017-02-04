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
                    UserTracker.updateGame(socketMessage.receiver, player1.game, player1.channel)
                    UserTracker.updateGame(socketMessage.sender, player1.game, player2.channel)
                    player1.game.map(g => {
                        val stateMessage = Pickle.intoString(g.currentState)
                        sendMessageToPlayers(GAME_UPDATE.id, 
                        socketMessage.receiver,
                        socketMessage.sender, stateMessage)
                    })
                } else if(socketMessage.messageType == WebSocketMessage.GAME_ACTION.id) {
                    Unpickle[ActionWrapper].fromString(socketMessage.data) match {
                        case Success(actionWrapper) => player1.game.map(g => {
                            val state = g.updateStateWrapper(actionWrapper)
                            val stateMessage = Pickle.intoString(state)
                            sendMessageToPlayers(GAME_UPDATE.id, 
                                socketMessage.receiver,
                                socketMessage.sender, stateMessage)
                            if(!state.gameState.equals("ongoing")) {
                                UserTracker.updateGame(
                                    socketMessage.receiver,
                                    player1.game.map(g => g.getClass.newInstance),
                                    player1.channel)
                                UserTracker.updateGame(
                                    socketMessage.sender,
                                    player1.game.map(g => g.getClass.newInstance),
                                    player2.channel)
                            }
                        })
                        case Failure(e) => UserTracker.sendTo(socketMessage.receiver, stringify(WebSocketMessage(
                                ERROR.id, socketMessage.sender, socketMessage.receiver, e.getMessage)))
                    }
                } else if(socketMessage.messageType == WebSocketMessage.GAME_SURRENDER.id) {
                    player1.game.map(g => {
                            val stateMessage = Pickle.intoString(g.currentState)
                            sendMessageToPlayers(GAME_UPDATE.id, 
                                socketMessage.receiver,
                                socketMessage.sender, stateMessage)
                    })
                }
            }
            UserTracker.sendTo(m, message)
        }
    }
    
    def sendMessageToPlayers(
        messageType : Int,
        player1 : String,
        player2 : String,
        message : String) {
        UserTracker.sendTo(player1, stringify(WebSocketMessage(
            messageType, player2, player1, message)))
        UserTracker.sendTo(player2, stringify(WebSocketMessage(
            messageType, player1, player2, message)))
    }

}
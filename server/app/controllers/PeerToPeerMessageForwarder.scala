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

class PeerToPeerMessageForwarder (val userService: UserService) {

    def forward(socketMessage : WebSocketMessage, player1 : UserRecord, player2 : UserRecord) {
        UserTracker.sendTo(socketMessage.receiver, stringify(socketMessage))
    }
    
    def resetGameSessions(socketMessage : WebSocketMessage, player1 : UserRecord, player2 : UserRecord) {
        UserTracker.updateGame(socketMessage.receiver,
            player1.game.map(g => g.getClass.newInstance),
            player1.channel)
        UserTracker.updateGame(
            socketMessage.sender,
            player1.game.map(g => g.getClass.newInstance),
            player2.channel)
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

trait ChallengeAcceptForwarder extends PeerToPeerMessageForwarder {
    override def forward (socketMessage : WebSocketMessage, player1 : UserRecord, player2 : UserRecord) {
        if (socketMessage.messageType == WebSocketMessage.CHALLENGE_ACCEPT.id) {
            UserTracker.sendTo(socketMessage.receiver, stringify(socketMessage))
            UserTracker.updateGame(socketMessage.receiver, player1.game, player1.channel)
            UserTracker.updateGame(socketMessage.sender, player1.game, player2.channel)
            player1.game.map(g => {
                val stateMessage = Pickle.intoString(g.currentState)
                sendMessageToPlayers(GAME_UPDATE.id,
                socketMessage.receiver,
                socketMessage.sender, stateMessage)
            })
        } else {
            super.forward(socketMessage, player1, player2)
        }
    }
}

trait GameActionForwarder extends PeerToPeerMessageForwarder {
    override def forward(socketMessage : WebSocketMessage, player1 : UserRecord, player2 : UserRecord) {
        if (socketMessage.messageType == WebSocketMessage.GAME_ACTION.id) {
            Unpickle[ActionWrapper].fromString(socketMessage.data) match {
                case Success(actionWrapper) => player1.game.map(g => {
                    val state = g.updateStateWrapper(actionWrapper)
                    val stateMessage = Pickle.intoString(state)
                    sendMessageToPlayers(GAME_UPDATE.id,
                        socketMessage.receiver,
                        socketMessage.sender, stateMessage)
                    if (!state.gameState.equals("ongoing")) {
                        resetGameSessions(socketMessage, player1, player2)
                    }
                })
                case Failure(e) => UserTracker.sendTo(socketMessage.receiver, stringify(WebSocketMessage(
                    ERROR.id, socketMessage.sender, socketMessage.receiver, e.getMessage)))
                }
        } else {
            super.forward(socketMessage, player1, player2)
        }
    }
}

trait GameSurrenderForwarder extends PeerToPeerMessageForwarder {
    override def forward(socketMessage : WebSocketMessage, player1 : UserRecord, player2 : UserRecord) {
        if (socketMessage.messageType == WebSocketMessage.GAME_SURRENDER.id) {
            UserTracker.sendTo(socketMessage.receiver, stringify(socketMessage))
            resetGameSessions(socketMessage, player1, player2)
        } else {
            super.forward(socketMessage, player1, player2)
        }
    }
}
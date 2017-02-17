package controllers

import service.UserService
import shared.WebSocketMessage.stringify
import shared.{ActionWrapper, WebSocketMessage, GameUpdateMessage,
               ChallengeAcceptMessage, GameActionMessage, GameSurrenderMessage, StateWrapper}

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
    
    def sendStateToPlayers(
        player1 : String,
        player2 : String,
        state : StateWrapper) {
        UserTracker.sendTo(player1, stringify(GameUpdateMessage(
            player2, player1, state)))
        UserTracker.sendTo(player2, stringify(GameUpdateMessage(
            player1, player2, state)))
    }

}

trait ChallengeAcceptForwarder extends PeerToPeerMessageForwarder {
    override def forward (socketMessage : WebSocketMessage,
        player1 : UserRecord, player2 : UserRecord) = socketMessage match {
        case challengeAccept : ChallengeAcceptMessage => {
            UserTracker.sendTo(challengeAccept.receiver, stringify(challengeAccept))
            UserTracker.updateGame(challengeAccept.receiver, player1.game, player1.channel)
            UserTracker.updateGame(challengeAccept.sender, player1.game, player2.channel)
            player1.game.map(g => {
                sendStateToPlayers(
                challengeAccept.receiver,
                challengeAccept.sender, g.currentState)
            })
        }
        case _ => super.forward(socketMessage, player1, player2)
    }
}

trait GameActionForwarder extends PeerToPeerMessageForwarder {
    override def forward(socketMessage : WebSocketMessage,
        player1 : UserRecord, player2 : UserRecord) = socketMessage match {
        case gameAction : GameActionMessage => {
            player1.game.map(g => {
                val state = g.updateStateWrapper(gameAction.action)
                sendStateToPlayers(
                    socketMessage.receiver,
                    socketMessage.sender, state)
                if (!state.gameState.equals("ongoing")) {
                    resetGameSessions(socketMessage, player1, player2)
                    userService.updateEloScore(socketMessage.sender, state.gameState, socketMessage.receiver)
                }
            })
        }
        case _ => super.forward(socketMessage, player1, player2)
    }
}

trait GameSurrenderForwarder extends PeerToPeerMessageForwarder {
    override def forward(socketMessage : WebSocketMessage,
        player1 : UserRecord, player2 : UserRecord) = socketMessage match {
        case gameSurrender : GameSurrenderMessage => {
            UserTracker.sendTo(socketMessage.receiver, stringify(socketMessage))
            resetGameSessions(socketMessage, player1, player2)
        }
        case _ => super.forward(socketMessage, player1, player2)
    }
}
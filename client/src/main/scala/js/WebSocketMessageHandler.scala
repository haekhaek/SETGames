package js

import shared.{StateWrapper, ActionWrapper}
import shared.WebSocketMessage
import shared.WebSocketMessage._
import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom
import org.scalajs.dom.html
import scalatags.JsDom.all._
import scala.scalajs.js.timers._
import prickle.{Pickle, Unpickle}

class WebSocketMessageHandler(
    val userName : String,
    val connection : dom.WebSocket) {

    def handle(message : WebSocketMessage) = {
        if(message.messageType == NOTIFICATION.id) {
            DomUtil.clearMessages
            DomUtil.displayMessage(DomMessage(
            s"${message.sender}: ",
            s"${message.data}",
            "info"))
        }
    }
}

trait ChallengeHandler extends WebSocketMessageHandler {
    override def handle(message : WebSocketMessage) = {
        if(message.messageType == CHALLENGE.id) {
            DomUtil.displayChallenge(connection, message)
            DomUtil.deactivateChallengeButtons(userName, connection)
        }
        super.handle(message)
    }
}

trait ChallengeAcceptHandler extends WebSocketMessageHandler {
    override def handle(message : WebSocketMessage) = {
        if(message.messageType == CHALLENGE_ACCEPT.id) {
            DomUtil.clearMessages
            DomUtil.displayMessage(DomMessage(
                s"${message.sender} ",
                "has accepted your challenge.",
                "warning"))
            DomUtil.setPlayingGame(true)
            DomUtil.deactivateChallengeButtons(userName, connection)
        }
        super.handle(message)
    }
}

trait ChallengeDeclinedHandler extends WebSocketMessageHandler {
    override def handle(message : WebSocketMessage) = {
        if(message.messageType == CHALLENGE_DECLINE.id) {
            DomUtil.clearMessages
            DomUtil.displayMessage(DomMessage(
                s"${message.sender} ",
                "has declined your challenge.",
                "warning"))
            DomUtil.setPlayingGame(false)
            DomUtil.activateChallengeButtons(userName, connection)
        }
        super.handle(message)
    }
}

trait UserUpdateHandler extends WebSocketMessageHandler {
    override def handle(message : WebSocketMessage) = {
        if(message.messageType == USER_UPDATE.id) {
            for {
                activeMembers <- Unpickle[Iterable[String]].fromString(message.data)
            } yield {
                DomUtil.updateMemberList(connection, userName, activeMembers)
            }
        }
        super.handle(message)
    }
}

trait ErrorHandler extends WebSocketMessageHandler {
    override def handle(message : WebSocketMessage) = {
        if(message.messageType == ERROR.id) {
            DomUtil.displayMessage(DomMessage("Error: ",
              s"${message.data}",
              "error"))
        }
        super.handle(message)
    }
}

trait TicTacToeUpdateHandler extends GameUpdateHandler {
    def activateUpdate(message: WebSocketMessage) = update(message, true)
    def deactivateUpdate(message: WebSocketMessage) = update(message, false)
    def setupUpdateLogic(message: WebSocketMessage) = ()
    def update(message: WebSocketMessage, myTurn: Boolean) = {
        for {
            state <- Unpickle[StateWrapper].fromString(message.data)
        } yield {
            TicTacToe.createGameField(state.field, myTurn, message)
        }
    }
}

trait FourWinsUpdateHandler extends GameUpdateHandler {
    def activateUpdate(message: WebSocketMessage) = update(message, true)
    def deactivateUpdate(message: WebSocketMessage) = update(message, false)
    def setupUpdateLogic(message: WebSocketMessage) = ()
    def update(message: WebSocketMessage, myTurn: Boolean) = {
        for {
            state <- Unpickle[StateWrapper].fromString(message.data)
        } yield {
            FourWins.createGameField(state.field, myTurn, message)
        }
    }
}

trait GameUpdateHandler extends WebSocketMessageHandler {

    def activateUpdate(message : WebSocketMessage) : Unit
    
    def deactivateUpdate(message : WebSocketMessage) : Unit
    
    def setupUpdateLogic(message : WebSocketMessage) : Unit

    override def handle(message : WebSocketMessage) = {
        if(message.messageType == GAME_UPDATE.id) {
            for {
                state <- Unpickle[StateWrapper].fromString(message.data)
            } yield {
                val myTurn = s"${state.playerLabel}".equals(DomUtil.currentPlayerLabel)
                val iWon = myTurn && state.gameState.equals("won")
                val iLost = !myTurn && state.gameState.equals("won")
                val draw = state.gameState.equals("even")
                deactivateUpdate(message)
                if(iWon) {
                    handleGameOver("Congratulations! ", "You have won this game!", "success")
                } else if(iLost) {
                    handleGameOver("Game Over! ", "You have lost!", "danger")
                } else if(draw) {
                    handleGameOver("It is a draw! ","No one has won or lost!","warning")
                } else if(myTurn) {
                    displayMyTurn
                    activateUpdate(message)
                    setupUpdateLogic(message)
                    DomUtil.setPlayingGame(true)
                } else {
                    displayNotMyTurn
                }
            }
        }
        super.handle(message)
    }

    def handleGameOver(caption : String, message : String, messageType : String): Unit = {
      DomUtil.clearMessages
      DomUtil.displayMessage(DomMessage(
        caption,
        message,
        messageType))
      DomUtil.activateChallengeButtons(userName, connection)
      DomUtil.setPlayingGame(false)
    }
    
    def displayMyTurn = {
      DomUtil.clearMessages
      DomUtil.displayMessage(DomMessage(
        "It's your turn",
        "",
        "info"))
    }
    
    def displayNotMyTurn = {
      DomUtil.clearMessages
      DomUtil.displayMessage(DomMessage(
        "It's your opponent's turn",
        "",
        "warning"))
    }
}
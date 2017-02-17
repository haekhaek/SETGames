package js

import shared.{StateWrapper, ActionWrapper, WebSocketMessage,
               GameUpdateMessage, ErrorMessage, UserUpdateMessage,
               ChallengeDeclineMessage, ChallengeAcceptMessage,
               ChallengeMessage, NotificationMessage, GameState}
import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom
import org.scalajs.dom.html
import scalatags.JsDom.all._
import scala.scalajs.js.timers._

class WebSocketMessageHandler(
    val userName : String,
    val connection : dom.WebSocket) {

    def handle(message : WebSocketMessage) = message match {
        case notification : NotificationMessage => {
            DomUtil.clearMessages
            DomUtil.displayMessage(DomMessage(
            s"${notification.sender}: ",
            s"${notification.content}",
            "info"))
        }
        case _ => ()
    }
}

trait ChallengeHandler extends WebSocketMessageHandler {
    override def handle(message : WebSocketMessage) = message match {
        case challenge : ChallengeMessage => {
            DomUtil.displayChallenge(connection, challenge)
            DomUtil.deactivateChallengeButtons(userName, connection)
        }
        case _ => super.handle(message)
    }
}

trait ChallengeAcceptHandler extends WebSocketMessageHandler {
    override def handle(message : WebSocketMessage) = message match {
        case challengeAccept : ChallengeAcceptMessage => {
            DomUtil.clearMessages
            DomUtil.displayMessage(DomMessage(
                s"${challengeAccept.sender} ",
                "has accepted your challenge.",
                "warning"))
            DomUtil.setPlayingGame(true)
            DomUtil.deactivateChallengeButtons(userName, connection)
        }
        case _ => super.handle(message)
    }
}

trait ChallengeDeclinedHandler extends WebSocketMessageHandler {
    override def handle(message : WebSocketMessage) = message match {
        case challengeDecline : ChallengeDeclineMessage => {
            DomUtil.clearMessages
            DomUtil.displayMessage(DomMessage(
                s"${challengeDecline.sender} ",
                "has accepted your challenge.",
                "warning"))
            DomUtil.setPlayingGame(true)
            DomUtil.deactivateChallengeButtons(userName, connection)
        }
        case _ => super.handle(message)
    }
}

trait UserUpdateHandler extends WebSocketMessageHandler {
    override def handle(message : WebSocketMessage) = message match {
        case u : UserUpdateMessage => DomUtil.updateMemberList(connection, userName, u.userList)
        case _ => super.handle(message)
    }
}

trait ErrorHandler extends WebSocketMessageHandler {
    override def handle(message : WebSocketMessage) = message match {
        case error : ErrorMessage => DomUtil.displayMessage(
              DomMessage("Error: ",
              s"${error.content}",
              "error"))
        case _ => super.handle(message)
    }
}

trait TicTacToeUpdateHandler extends GameUpdateHandler {
    def activateUpdate(message: GameUpdateMessage) = update(message, true)
    def deactivateUpdate(message: GameUpdateMessage) = update(message, false)
    def update(message: GameUpdateMessage, myTurn: Boolean) = {
        TicTacToe.createGameField(message.state.field, myTurn, message)
    }
}

trait FourWinsUpdateHandler extends GameUpdateHandler {
    def activateUpdate(message: GameUpdateMessage) = update(message, true)
    def deactivateUpdate(message: GameUpdateMessage) = update(message, false)
    def update(message: GameUpdateMessage, myTurn: Boolean) = {
        ConnectFour.createGameField(message.state.field, myTurn, message)
    }
}

trait BattleShipUpdateHandler extends GameUpdateHandler {
    def activateUpdate(message: GameUpdateMessage) = update(message, true)
    def deactivateUpdate(message: GameUpdateMessage) = update(message, false)
    def update(message: GameUpdateMessage, myTurn: Boolean) = {
        BattleShip.createGameField(message.state.field, myTurn, message)
    }
}

trait GameUpdateHandler extends WebSocketMessageHandler {

    def activateUpdate(message : GameUpdateMessage) : Unit
    
    def deactivateUpdate(message : GameUpdateMessage) : Unit

    override def handle(message : WebSocketMessage) = message match {
        case gameUpdate : GameUpdateMessage => {
            val state = gameUpdate.state
            val myTurn = s"${state.playerLabel}".equals(DomUtil.currentPlayerLabel)
            val iWon = myTurn && state.gameState.equals(GameState.WON.toString)
            val iLost = !myTurn && state.gameState.equals(GameState.WON.toString)
            val draw = state.gameState.equals(GameState.EVEN.toString)
            deactivateUpdate(gameUpdate)
            if(iWon) {
                handleGameOver("Congratulations! ", "You have won this game!", "success")
            } else if(iLost) {
                handleGameOver("Game Over! ", "You have lost!", "danger")
            } else if(draw) {
                handleGameOver("It is a draw! ","No one has won or lost!","warning")
            } else if(myTurn) {
                displayMyTurn
                activateUpdate(gameUpdate)
                DomUtil.setPlayingGame(true)
            } else {
                displayNotMyTurn
            }
        }
        case _ => super.handle(message)
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
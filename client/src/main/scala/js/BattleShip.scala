package js

import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.raw.HTMLImageElement
import org.scalajs.dom.raw.HTMLDivElement
import scalatags.JsDom.all._
import scala.scalajs.js.annotation.JSExport
import shared.{WebSocketMessage,ActionWrapper, GameActionMessage}
import shared.WebSocketMessage._

@JSExport
object BattleShip{
  val gameFieldIdX: String = "gameFieldBattleShipX"
  val gameFieldIdO: String = "gameFieldBattleShipO"
  val gameContainerId: String = "gameContainerBattleShip"
  val blockPrefix: String = "battleShipBlockId-"
  val cssGameBlockClass: String = "gameBlockBattleShip"
  var userName: String = ""
  var connection: dom.WebSocket = null
  var myTurn: Boolean = false

  //force browser to download images
  val playerYellowImg = new Image("/assets/images/battleship.png", "playerImage")
  val playerRedImg = new Image("/assets/images/battleship-hit.png", "playerImage")
  val gameFieldImg = new Image("/assets/images/battleship-water.png", "gameFieldImage")

  def clickedEvent(elementClicked:dom.Event, message: WebSocketMessage): Unit = {
    myTurn match {
      case true => processClick(elementClicked, message)
      case _ => println("It is not your turn")
    }
  }

  def processClick(elementClicked:dom.Event, message: WebSocketMessage): Unit = {
      val myBlock = elementClicked.target.asInstanceOf[HTMLDivElement]
      val blockIdClicked = myBlock.id.replace(blockPrefix, "")

      //parse coordinates to int
      val x = blockIdClicked.charAt(0)-'0'
      val y = blockIdClicked.charAt(1)-'0'

      val action = ActionWrapper(List(x,y))
      connection.send(stringify(GameActionMessage(userName, message.sender, action)))
      myTurn = false
  }

  @JSExport
  def startGame(
    userName_ : String,
    send : html.Button,
    message : html.Input,
    connection_ : dom.WebSocket
  ) {
    userName = userName_
    connection = connection_

    val onConnectionOpenedHandler = DomUtil.setupShoutMessenger(userName, send, message, connection)
    WebSocketUtil
      .setup(connection
            ,onConnectionOpenedHandler
            ,new WebSocketMessageHandler(userName, connection)
               with ChallengeHandler
               with ChallengeAcceptHandler
               with ChallengeDeclinedHandler
               with UserUpdateHandler
               //with ErrorHandler
               with BattleShipUpdateHandler
            )
  }

  @JSExport
  def createGameField(
    playerCharacters : Iterable[Iterable[Char]],
    myTurn_ : Boolean,
    message : WebSocketMessage
  ){

    myTurn = myTurn_
    val gameContainer = dom.document.getElementById(gameContainerId).asInstanceOf[HTMLDivElement]
    gameContainer.innerHTML = ""

    val half = (playerCharacters size) / 2
    val (fieldX, fieldO) = playerCharacters.splitAt(half)
    val myPlayerLabel = DomUtil currentPlayerLabel

    myPlayerLabel match {
      case "X" =>
        gameContainer appendChild(foo(fieldX, gameFieldIdX, myTurn, message, true))
        gameContainer appendChild(foo(fieldO, gameFieldIdO, myTurn, message, false))
      case "O" =>
        gameContainer appendChild(foo(fieldO, gameFieldIdO, myTurn, message, true))
        gameContainer appendChild(foo(fieldX, gameFieldIdX, myTurn, message, false))
      case _ => println("error")
    }
  }

  def foo(
    field: Iterable[Iterable[Char]],
    myGameFieldId: String,
    myTurn: Boolean,
    message: WebSocketMessage,
    showShips: Boolean): HTMLDivElement = {

    val gameField: HTMLDivElement = div(cls:="gameFieldBattleShip",id:=myGameFieldId).render

    for(i <- field.view.zipWithIndex.toList.reverse){
      for(player <- i._1.view.zipWithIndex){
        val y = i._2
        val x = player._2

        if(player._1 == 'B' && showShips){
          gameField.appendChild(
            createDivWithImage(y+""+x, message, "/assets/images/battleship.png")
          )
        }else if(player._1 == 'W'){
          gameField.appendChild(
            createDivWithImage(y+""+x, message, "/assets/images/battleship-water.png")
          )
        }else if(player._1 == 'H'){
          gameField.appendChild(
            createDivWithImage(y+""+x, message, "/assets/images/battleship-hit.png")
          )
        }else{
          gameField.appendChild(createDiv(y+""+x, message))
        }
      }
    }

    gameField
  }

  def createDivWithImage (x: String, message: WebSocketMessage, imageSrc: String) = 
  div(
    cls:=s"battleShipBlock",
    id:=s"battleShipBlockId-$x",
    onclick := { (e: dom.Event) => BattleShip.clickedEvent(e, message) },
    img(src:=imageSrc, cls:="playerImage")
  ).render
  
  def createDiv(x: String, message: WebSocketMessage) =
  div(
    cls:=s"battleShipBlock",
    id:=s"battleShipBlockId-$x",
    onclick := {(e: dom.Event) => BattleShip.clickedEvent(e, message)}
  ).render

}
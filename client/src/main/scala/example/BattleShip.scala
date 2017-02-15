package example

import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.raw.HTMLImageElement
import org.scalajs.dom.raw.HTMLDivElement
import scalatags.JsDom.all._
import scala.scalajs.js.annotation.JSExport
import shared.{WebSocketMessage,ActionWrapper}
import shared.WebSocketMessage._
import prickle.Pickle

@JSExport
object BattleShip{
  val gameFieldIdX: String = "gameFieldBattleShipX"
  val gameFieldIdO: String = "gameFieldBattleShipO"
  val gameContainerId: String = "gameContainerBattleShip"
  val blockPrefix: String = "battleShipBlockId-"
  val cssGameBlockClass: String = "gameBlockBattleShip"
  var blockIds = for (i <- 0 to 9; j <- 0 to 9) yield i+""
  var userName: String = ""
  var connection: dom.WebSocket = null
  var canIClick: Boolean = false

  //force browser to download images
  val playerYellowImg = new Image("/assets/images/battleship.png", "playerImage")
  val playerRedImg = new Image("/assets/images/battleship-hit.png", "playerImage")
  val gameFieldImg = new Image("/assets/images/battleship-water.png", "gameFieldImage")

  def clickedOnBlock(elementClicked:dom.Event
                    ,message: WebSocketMessage): Unit = {
    if (canIClick){
      val myBlock = elementClicked.target.asInstanceOf[HTMLDivElement]
      val blockIdClicked = myBlock.id.replace(blockPrefix, "")

      //parse coordinates to int
      val x = blockIdClicked.charAt(0)-'0'
      val y = blockIdClicked.charAt(1)-'0'

      val action = ActionWrapper(List(x,y))
      connection.send(stringify(WebSocketMessage(GAME_ACTION.id, userName, message.sender, Pickle.intoString(action))))
      canIClick = false
    }
  }

  def setImageToBlock(blockId: String, player: String): Unit = {
    val block = dom.document.getElementById(blockPrefix+blockId).asInstanceOf[HTMLDivElement]
    player match {
      case "x" => block.appendChild(new Image("/assets/images/x-player.png", "playerImage").element)
      case _ => block.appendChild(new Image("/assets/images/o-player.png", "playerImage").element)
    }
  }

  def updateBlockIds(blockId: String): scala.collection.immutable.IndexedSeq[String] = {
    blockIds.filter(!_.equals(blockId))
  }

  @JSExport
  def startGame(
        userName_ : String,
        send : html.Button,
        message : html.Input,
        connection_ : dom.WebSocket) {
    userName = userName_
    connection = connection_

    val onConnectionOpenedHandler = 
        DomUtil.setupShoutMessenger(userName, send, message, connection)
    WebSocketUtil.setup(
        connection,
        onConnectionOpenedHandler,
        new WebSocketMessageHandler(userName, connection)
            with ChallengeHandler
            with ChallengeAcceptHandler
            with ChallengeDeclinedHandler
            with UserUpdateHandler
            //with ErrorHandler
            with BattleShipUpdateHandler
        )
  }

  @JSExport
  def createGameField(playerCharacters: Iterable[Iterable[Char]]
                     ,myTurn: Boolean
                     ,message: WebSocketMessage
                     ){
    canIClick = myTurn
    val gameContainer = dom.document.getElementById(gameContainerId).asInstanceOf[HTMLDivElement]
    gameContainer.innerHTML = ""

    val half = (playerCharacters size) / 2
    val (fieldX, fieldO) = playerCharacters.splitAt(half)
    var gameFieldX = createDiv("", "")
    var gameFieldO = createDiv("", "")

    val myPlayerLabel = dom.document.getElementById("currentPlayerLabel").innerHTML
    println(myPlayerLabel)

    myPlayerLabel match {
      case "X" =>
        gameFieldX = foo(fieldX, gameFieldIdX, myTurn, message, true)
        gameFieldO = foo(fieldO, gameFieldIdO, myTurn, message, false)
      case "O" =>
        gameFieldX = foo(fieldX, gameFieldIdX, myTurn, message, false)
        gameFieldO = foo(fieldO, gameFieldIdO, myTurn, message, true)
      case _ => println("error")
    }
    gameContainer appendChild(gameFieldX)
    gameContainer appendChild(gameFieldO)
  }

  def foo(field: Iterable[Iterable[Char]]
         ,myGameFieldId: String
         ,myTurn: Boolean
         ,message: WebSocketMessage
         ,showShips: Boolean): HTMLDivElement = {

    val gameField: HTMLDivElement = createDiv("gameFieldBattleShip", myGameFieldId)

    for(i <- field.view.zipWithIndex.toList.reverse){
      for(player <- i._1.view.zipWithIndex){
        val y = i._2
        val x = player._2

        if(player._1 == 'B' && showShips){
          val imageSrc = Some("/assets/images/battleship.png")
          val block = createGameBlock(imageSrc, y+""+x, myTurn, message)

          gameField.appendChild(block)
        }else if(player._1 == 'W'){
          val imageSrc = Some("/assets/images/battleship-water.png")
          val block = createGameBlock(imageSrc, y+""+x, myTurn, message)
          gameField.appendChild(block)
        }else if(player._1 == 'H'){
          val imageSrc = Some("/assets/images/battleship-hit.png")
          val block = createGameBlock(imageSrc, y+""+x, myTurn, message)
          gameField.appendChild(block)
        }else{
          gameField.appendChild(createGameBlock(None, y+""+x, myTurn, message))
        }
      }
    }

    gameField
  }

  def createDiv(cssClass:String, cssId: String) = div(
      cls:=cssClass,
      id:=cssId
    ).render

  def createGameBlock(image: Option[String]
                     ,x: String
                     ,myTurn: Boolean
                     ,message: WebSocketMessage) =
   image match {
    case None => {
      div(cls:=s"battleShipBlock"
         ,id:=s"battleShipBlockId-$x"
         ,onclick := {
          (e: dom.Event) => clickedOnBlock(e, message)
         }
      ).render
    }
    case Some (imageSrc) => {
      div(cls:=s"battleShipBlock"
         ,id:=s"battleShipBlockId-$x"
         ,onclick := {
            (e: dom.Event) => {
              if(myTurn){
                clickedOnBlock(e, message)
              }
            }
          }
        ,img(src:=imageSrc, cls:="playerImage")
      ).render
    }
  }

}
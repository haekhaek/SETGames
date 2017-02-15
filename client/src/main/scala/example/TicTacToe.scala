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

class Image(src: String, cssClassName: String) {
  private var ready: Boolean = false

  val element = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
  element.onload = (e: dom.Event) => ready = true
  element.src = src
  element.className = cssClassName

  def isReady: Boolean = ready
}

@JSExport
object TicTacToe{
  val gameFieldId: String = "gameField"
  val gameContainerId: String = "gameContainer"
  val blockPrefix: String = "blockId-"
  var blockIds = for (i <- 0 to 2; j <- 0 to 2) yield i+""+j
  var userName: String = ""
  var connection: dom.WebSocket = null
  var canIClick: Boolean = false

  //force browser to download images
  val playerXImage = new Image("/assets/images/x-player.png", "playerImage")
  val playerOImage = new Image("/assets/images/o-player.png", "playerImage")
  val gameFieldImage = new Image("/assets/images/ttt-background-red.jpg", "gameFieldImage")

  def clickedOnBlock(elementClicked:dom.Event, message: WebSocketMessage): Unit = {
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
            with TicTacToeUpdateHandler
        )
  }

  @JSExport
	def createGameField(playerCharacters: Iterable[Iterable[Char]], myTurn: Boolean, message: WebSocketMessage){
    canIClick = myTurn
    val gameField: HTMLDivElement = createDiv("", gameFieldId)
    val gameContainer = dom.document.getElementById(gameContainerId).asInstanceOf[HTMLDivElement]
    gameContainer.innerHTML = ""

    for(i <- playerCharacters.view.zipWithIndex){
      for(player <- i._1.view.zipWithIndex){
        val y = i._2
        val x = player._2

        if(player._1 == 'X'){
          val imageSrc = Some("/assets/images/x-player.png")
          val block = createGameBlock(imageSrc, y+""+x, myTurn ,message)

          gameField.appendChild(block)
        }else if(player._1 == 'O'){
          val imageSrc = Some("/assets/images/o-player.png")
          val block = createGameBlock(imageSrc, y+""+x, myTurn, message)
          gameField.appendChild(block)
        }else{
          gameField.appendChild(createGameBlock(None, y+""+x, myTurn,message))
        }
      }
    }
    gameContainer.appendChild(gameField)
  }

  def createDiv(cssClass:String, cssId: String) = div(
      cls:=cssClass,
      id:=cssId
    ).render

  def createGameBlock(image: Option[String], x: String, myTurn: Boolean, message: WebSocketMessage) = 
      image match {
        case None => {
          div(
            cls:=s"gameBlock",
            id:=s"blockId-$x",
            onclick := {
            (e: dom.Event) => clickedOnBlock(e, message)
            }
          ).render
        }
        case Some (imageSrc) => {
          div(
            cls:=s"gameBlock",
            id:=s"blockId-$x",
            onclick := {
            (e: dom.Event) => {
              if(myTurn){
                clickedOnBlock(e, message)
              }
              }
            },
            img(src:=imageSrc, cls:="playerImage")
          ).render
        }
      }
}
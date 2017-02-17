package js

import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.raw.HTMLImageElement
import org.scalajs.dom.raw.HTMLDivElement
import scalatags.JsDom.all._
import scala.scalajs.js.annotation.JSExport
import shared.{WebSocketMessage,ActionWrapper, GameActionMessage}
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

trait ActionWrapperMaker {
  def make(blockIdClicked: String): ActionWrapper
}

trait ActionWrapperMakerConnectFour extends ActionWrapperMaker{
  val blockPrefix: String = "blockId-"

  override def make(blockIdClicked: String) = {
    val columnClicked = blockIdClicked.replace(blockPrefix, "").toInt
    ActionWrapper(List(columnClicked))
  }
}

trait ActionWrapperMakerTicTacToe extends ActionWrapperMaker{
  val blockPrefix: String = " blockId-"

  override def make(blockIdClicked: String) = {
    val foo = blockIdClicked.replace("blockId-", "")

    //parse coordinates to int
    val x = foo.charAt(0)-'0'
    val y = foo.charAt(1)-'0'
    ActionWrapper(List(x,y))
  }
}

trait GameFieldMaker {
  val gameContainerId: String
  def makeGameField(
    playerCharacters: Iterable[Iterable[Char]],
    createGameBlock: (Option[String], String) => HTMLDivElement
    ): HTMLDivElement
}

trait GameFieldMakerConnectFour extends GameFieldMaker {
  val gameFieldId: String = "gameFieldFourWins"
  override val gameContainerId: String = "gameContainerFourWins"

  val playerYellowImg = new Image("/assets/images/four-wins-player-yellow.jpg", "playerImage")
  val playerRedImg = new Image("/assets/images/four-wins-player-red.jpg", "playerImage")
  val gameFieldImg = new Image("/assets/images/four-wins-block.jpg", "gameFieldImage")

  override def makeGameField(
    playerCharacters: Iterable[Iterable[Char]]
   ,createGameBlock: (Option[String],String) => HTMLDivElement): HTMLDivElement = {

    val gameField: HTMLDivElement = div(cls:="",id:=gameFieldId).render

    for(i <- playerCharacters.view.zipWithIndex.toList.reverse){
      for(player <- i._1.view.zipWithIndex){
        val y = i._2
        val x = player._2

        if(player._1 == 'X'){
          val imageSrc = Some("/assets/images/four-wins-player-yellow.jpg")
          val block = createGameBlock(imageSrc, (x toString))

          gameField.appendChild(block)
        }else if(player._1 == 'O'){
          val imageSrc = Some("/assets/images/four-wins-player-red.jpg")
          val block = createGameBlock(imageSrc, (x toString))
          gameField.appendChild(block)
        }else{
          gameField.appendChild(createGameBlock(None, (x toString)))
        }
      }
    }

    gameField
  }
}

trait GameFieldMakerTicTacToe extends GameFieldMaker {
  val gameFieldId: String = "gameField"
  override val gameContainerId: String = "gameContainer"

  //force browser to download images
  val playerXImage = new Image("/assets/images/x-player.png", "playerImage")
  val playerOImage = new Image("/assets/images/o-player.png", "playerImage")
  val gameFieldImage = new Image("/assets/images/ttt-background-red.jpg", "gameFieldImage")

  override def makeGameField(
    playerCharacters: Iterable[Iterable[Char]]
   ,createGameBlock: (Option[String],String) => HTMLDivElement): HTMLDivElement = {

    val gameField: HTMLDivElement = div(cls:="",id:=gameFieldId).render

    for(i <- playerCharacters.view.zipWithIndex){
      for(player <- i._1.view.zipWithIndex){
        val y = i._2
        val x = player._2

        if(player._1 == 'X'){
          val imageSrc = Some("/assets/images/x-player.png")
          val block = createGameBlock(imageSrc, y+""+x)

          gameField.appendChild(block)
        }else if(player._1 == 'O'){
          val imageSrc = Some("/assets/images/o-player.png")
          val block = createGameBlock(imageSrc, y+""+x)
          gameField.appendChild(block)
        }else{
          gameField.appendChild(createGameBlock(None, y+""+x))
        }
      }
    }

    gameField
  }
}

trait Game {
  this: ActionWrapperMaker with GameFieldMaker =>

  val blockPrefix: String
  var userName: String = ""
  var connection: dom.WebSocket = null
  var myTurn: Boolean = false

  def clickedOnBlock(elementClicked:dom.Event, message: WebSocketMessage): Unit = {
    if (myTurn){
      val myBlock = elementClicked.target.asInstanceOf[HTMLDivElement]
      val action = this.make(myBlock.className)
      connection.send(stringify(GameActionMessage(userName, message.sender, action)))
      myTurn = false
    }
  }

  def startGame(
        myUserName : String,
        send : html.Button,
        message : html.Input,
        myConnection : dom.WebSocket) {

    userName = myUserName
    connection = myConnection
  }

  def createGameField(playerCharacters: Iterable[Iterable[Char]], myTurn_ : Boolean, message: WebSocketMessage){
    myTurn = myTurn_
    val gameContainer = dom.document.getElementById(gameContainerId).asInstanceOf[HTMLDivElement]
    gameContainer.innerHTML = ""
    val gameField = this.makeGameField(playerCharacters, createGameBlock(_, _, myTurn, message))
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
          cls:=s"blockId-$x",
          // id:=s"blockId-$x$y",
          onclick := {
          (e: dom.Event) => clickedOnBlock(e, message)
          }
        ).render
      }
      case Some (imageSrc) => {
        div(
          cls:=s"blockId-$x",
          // id:=s"blockId-$x"+y,
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

@JSExport
object ConnectFour {
  val game = new Game with ActionWrapperMakerConnectFour with GameFieldMakerConnectFour {}

  @JSExport
  def createGameField(
    playerCharacters: Iterable[Iterable[Char]],
    myTurn: Boolean,
    message: WebSocketMessage):Unit = game.createGameField(playerCharacters, myTurn, message)

    @JSExport
  def startGame(
        userName : String,
        send : html.Button,
        message : html.Input,
        connection : dom.WebSocket) {

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
            with FourWinsUpdateHandler
        )
    game.startGame(userName, send, message, connection)
  }
}


@JSExport
object TicTacToe {
  val game = new Game with ActionWrapperMakerTicTacToe with GameFieldMakerTicTacToe {}

  @JSExport
  def createGameField(
    playerCharacters: Iterable[Iterable[Char]],
    myTurn: Boolean,
    message: WebSocketMessage):Unit = game.createGameField(playerCharacters, myTurn, message)

    @JSExport
  def startGame(
        userName : String,
        send : html.Button,
        message : html.Input,
        connection : dom.WebSocket) {

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
            with TicTacToeUpdateHandler
        )
    game.startGame(userName, send, message, connection)
  }
}
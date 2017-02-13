package js

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
object FourWins{
  val gameFieldId: String = "gameFieldFourWins"
  val gameContainerId: String = "gameContainerFourWins"
  val blockPrefix: String = "fourWinsBlockId-"
  val cssGameBlockClass: String = "gameBlockFourWins"
  var blockIds = for (i <- 0 to 6; j <- 0 to 5) yield i+""
  var userName: String = ""
  var connection: dom.WebSocket = null
  var canIClick: Boolean = false

  //force browser to download images
  val playerYellowImg = new Image("/assets/images/four-wins-player-yellow.jpg", "playerImage")
  val playerRedImg = new Image("/assets/images/four-wins-player-red.jpg", "playerImage")
  val gameFieldImg = new Image("/assets/images/four-wins-block.jpg", "gameFieldImage")

  def clickedOnBlock(elementClicked:dom.Event, message: WebSocketMessage): Unit = {
    if (canIClick){
      val myBlock = elementClicked.target.asInstanceOf[HTMLDivElement]
      val columnClicked = myBlock.className.replace(blockPrefix, "").toInt
      println(columnClicked)

      val action = ActionWrapper(List(columnClicked))
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
  def createDummyData(): Iterable[Iterable[Char]] = {
    List(
      List('-','-','-','-','-','-','-'),
      List('-','-','-','-','-','-','-'),
      List('-','-','-','-','-','-','-'),
      List('-','-','-','-','-','-','-'),
      List('-','-','-','-','-','-','-'),
      List('-','-','-','-','-','-','-')
    )
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
            with FourWinsUpdateHandler
        )
  }

  @JSExport
  def createGameField(playerCharacters: Iterable[Iterable[Char]], myTurn: Boolean, message: WebSocketMessage){
    canIClick = myTurn
    val gameField: HTMLDivElement = createDiv("", gameFieldId)
    val gameContainer = dom.document.getElementById(gameContainerId).asInstanceOf[HTMLDivElement]
    gameContainer.innerHTML = ""

    println(playerCharacters)

    for(i <- playerCharacters.view.zipWithIndex.toList.reverse){
      for(player <- i._1.view.zipWithIndex){
        val y = i._2
        val x = player._2

        if(player._1 == 'X'){
          val imageSrc = Some("/assets/images/four-wins-player-yellow.jpg")
          val block = createGameBlock(imageSrc, (x toString), myTurn, message)

          gameField.appendChild(block)
        }else if(player._1 == 'O'){
          val imageSrc = Some("/assets/images/four-wins-player-red.jpg")
          val block = createGameBlock(imageSrc, (x toString), myTurn, message)
          gameField.appendChild(block)
        }else{
          gameField.appendChild(createGameBlock(None, (x toString), myTurn, message))
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
            cls:=s"fourWinsBlockId-$x",
            // id:=s"blockId-$x$y",
            onclick := {
            (e: dom.Event) => clickedOnBlock(e, message)
            }
          ).render
        }
        case Some (imageSrc) => {
          div(
            cls:=s"fourWinsBlockId-$x",
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
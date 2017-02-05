package example

import org.scalajs.dom
import org.scalajs.dom.raw.HTMLImageElement
import org.scalajs.dom.raw.HTMLDivElement
import scalatags.JsDom.all._
import scala.scalajs.js.annotation.JSExport

class Image(src: String, cssClassName: String) {
  private var ready: Boolean = false

  val element = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
  element.onload = (e: dom.Event) => ready = true
  element.src = src
  element.className = cssClassName

  def isReady: Boolean = ready
}

@JSExport
object TestTicTacToe{
  val gameFieldId: String = "gameField"
  val gameContainerId: String = "gameContainer"
  val blockPrefix: String = "blockId-"
  var blockIds = for (i <- 0 to 2; j <- 0 to 2) yield i+""+j

  //force browser to download images
  val playerXImage = new Image("/assets/images/x-player.png", "playerImage")
  val playerOImage = new Image("/assets/images/o-player.png", "playerImage")
  val gameFieldImage = new Image("/assets/images/ttt-background-red.jpg", "gameFieldImage")

  def clickedOnBlock(elementClicked:dom.Event): Unit = {
    val myBlock = elementClicked.target.asInstanceOf[HTMLDivElement]
    val blockIdClicked = myBlock.id.replace(blockPrefix, "")

    if(blockIds.contains(blockIdClicked)){
      blockIds = updateBlockIds(blockIdClicked)
      setImageToBlock(blockIdClicked, "x")

      println(blockIdClicked)
      println(blockIds)
    }else{
      println("Block already taken")
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
	def createGameField(){
    val gameField: HTMLDivElement = createDiv("", gameFieldId)
    val gameContainer = dom.document.getElementById(gameContainerId).asInstanceOf[HTMLDivElement]

    for (x <- blockIds) {
      gameField.appendChild(createGameBlock(x))
    }
    gameContainer.appendChild(gameField)
  }

  def createDiv(cssClass:String, cssId: String) = div(
      cls:=cssClass,
      id:=cssId
    ).render

  def createGameBlock(x: String) = div(
      cls:=s"gameBlock",
      id:=s"blockId-$x",
      onclick := {
        (e: dom.Event) => clickedOnBlock(e)
      }
    ).render
}
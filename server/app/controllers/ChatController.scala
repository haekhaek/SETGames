package controllers

import java.net.URL
import javax.inject._

import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.{BroadcastHub, Flow, Keep, MergeHub, Source}
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}
import scala.collection.mutable.{Map, HashMap}

/**
 * A very simple chat client using websockets.
 */
@Singleton
class ChatController @Inject()(implicit actorSystem: ActorSystem,
                               mat: Materializer,
                               executionContext: ExecutionContext)
  extends Controller {

  private type WSMessage = String

  // chat room many clients -> merge hub -> broadcasthub -> many clients
  private val (chatSink, chatSource) = {
    // Don't log MergeHub$ProducerFailed as error if the client disconnects.
    // recoverWithRetries -1 is essentially "recoverWith"
    val source = MergeHub.source[WSMessage]
      .log("source")
      .recoverWithRetries(-1, { case _: Exception ⇒ Source.empty })

    val sink = BroadcastHub.sink[WSMessage]
    source.toMat(sink)(Keep.both).run()
  }

  private val userFlow: Flow[WSMessage, WSMessage, _] = {
     Flow.fromSinkAndSource(chatSink, chatSource).log("userFlow")
  }

  def chat: Action[AnyContent] = Action { implicit request =>
    val url = routes.ChatController.chatChannel().webSocketURL()
    val clientAddress = request.remoteAddress
    UserTracker.update(clientAddress)
    Ok(views.html.chat(url, clientAddress, UserTracker.currentlyActiveUsers))
        .withSession(request.session + ("userId" -> clientAddress))
  }

  def chatChannel: WebSocket = {
    WebSocket.acceptOrResult[WSMessage, WSMessage] {
      case rh =>
        Future.successful(userFlow).map { flow =>
          Right(flow)
        }.recover {
          case e: Exception =>
            val msg = "Cannot create websocket"
            val result = InternalServerError(msg)
            Left(result)
        }
    }
  }

}

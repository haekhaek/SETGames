package controllers

import javax.inject._

import play.api.mvc._

import scala.concurrent.ExecutionContext

/**
 * A very simple chat client using websockets.
 */
@Singleton
class ChatController @Inject()(executionContext: ExecutionContext) extends Controller {

  def chat: Action[AnyContent] = Action { implicit request =>
    val url = routes.WebSocketController.websocket().webSocketURL()
    val clientAddress = request.remoteAddress
    Ok(views.html.chat(url, clientAddress, UserTracker.currentlyActiveUsers))
        .withSession(request.session + ("userId" -> clientAddress))
  }

}

package controllers

import javax.inject._

import shared.WebSocketMessage
import akka.actor._
import akka.stream.Materializer
import play.api.mvc._
import play.api.libs.streams.ActorFlow
import scala.concurrent.ExecutionContext

@Singleton
class GameController @Inject()(implicit actorSystem: ActorSystem,
                               mat: Materializer,
                               executionContext: ExecutionContext) extends Controller {

  def tictactoe = Action { implicit request =>
      val loggedIn = request.session.get("userName")
      val userId = loggedIn.get
      UserTracker.updateGame(userId, Some(null))
      val url = routes.WebSocketController
        .websocket().webSocketURL()
      Ok(views.html.tictactoe(userId, url))
  }

}

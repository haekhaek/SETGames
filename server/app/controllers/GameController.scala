package controllers

import javax.inject._

import akka.actor._
import akka.stream.Materializer
import model.TicTacToeGame
import play.api.mvc._

import scala.concurrent.ExecutionContext

@Singleton
class GameController @Inject()(implicit actorSystem: ActorSystem,
                               mat: Materializer,
                               executionContext: ExecutionContext) extends Controller {

  def tictactoe = Action { implicit request =>
      val loggedIn = request.session.get("userName")
      val userId = loggedIn.get

      UserTracker.updateGame(userId, Some(new TicTacToeGame()))
      val url = routes.WebSocketController
        .websocket().webSocketURL()
      Ok(views.html.tictactoe(userId, url))
  }
}

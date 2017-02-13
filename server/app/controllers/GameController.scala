package controllers

import javax.inject._
import model.{StupidButtonGame, TicTacToeGame}
import akka.actor._
import akka.stream.Materializer
import play.api.mvc._
import scala.concurrent.ExecutionContext

@Singleton
class GameController @Inject()(implicit actorSystem: ActorSystem,
                               mat: Materializer,
                               executionContext: ExecutionContext,
                               val auth: AuthAction) extends Controller {

  def tictactoe = auth.AuthenticatedAction { implicit request =>
      val userId = request.session.get("userName").get

      UserTracker.updateGame(userId, Some(new TicTacToeGame()))
      val url = routes.WebSocketController
        .websocket().webSocketURL()
      Ok(views.html.tictactoe(userId, url))
  }

  def fourwins = auth.AuthenticatedAction { implicit request =>
    val userId = request.session.get("userName").get

    // UserTracker.updateGame(userId, Some(new TicTacToeGame()))
    val url = routes.WebSocketController
      .websocket().webSocketURL()
    Ok(views.html.fourwins(userId, url))
  }

  def stupidbutton = auth.AuthenticatedAction { implicit request =>
    val userId = request.session.get("userName").get

    UserTracker.updateGame(userId, Some(new StupidButtonGame()))
    val url = routes.WebSocketController
      .websocket().webSocketURL()
    Ok(views.html.stupidbutton(userId, url))
  }

}

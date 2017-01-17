package controllers

import model.{User, Users, UserForm}
import play.api.mvc._
import scala.concurrent.Future
import service.UserService
import scala.concurrent.ExecutionContext.Implicits.global
import javax.inject.Inject
import javax.inject.Singleton

//class Application extends Controller {
@Singleton class Application @Inject() (users: Users, userService: UserService) extends Controller {
    def index = Action.async { implicit request =>
      userService.listAllUsers map { users =>
        val url = routes.WebSocketController.websocket().webSocketURL()
        Ok(views.html.index(UserForm.form, users, url))
      }
    }

    def addUser() = Action.async { implicit request =>
      val url = routes.WebSocketController.websocket().webSocketURL()
      UserForm.form.bindFromRequest.fold(
        // if any error in submitted data
        errorForm => Future.successful(Ok(views.html.index(errorForm, Seq.empty[User], url))),
        data => {
          val newUser = User(0, data.firstName, data.lastName, data.email)
          userService.addUser(newUser).map(res =>
            Redirect(routes.Application.index())
          )
        })
    }

    def deleteUser(id: Long) = Action.async { implicit request =>
      userService.deleteUser(id) map { res =>
        Redirect(routes.Application.index())
      }
    }
}

//  def index = Action {
//    Ok(views.html.index(SharedMessages.itWorks))
//  }
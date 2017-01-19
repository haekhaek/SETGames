package controllers

import javax.inject.Inject

import model.{LoginForm, UserForm, User, Users}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc._
import service.UserService

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

//class Application extends Controller {
class Application @Inject()(val messagesApi: MessagesApi, users: Users, userService: UserService)
  extends Controller with I18nSupport{
/*    def index = Action.async { implicit request =>
      userService.listAllUsers map { users =>
        Ok(views.html.index(UserForm.form, users))
      }
    }*/

  def index = Action { implicit request =>
    val loggedIn = request.session.get("userName") //TODO
    //val loggedIn = None
    if (loggedIn != None){
      Ok(views.html.home(loggedIn.get))
    }
    else
      Ok(views.html.login(LoginForm.form))
  }


  def login() = Action.async { implicit request =>
    LoginForm.form.bindFromRequest.fold(
      errorForm => Future.successful(Ok("login failed")),
      data => {
           Future.successful(Redirect(routes.Application.index())
             .withSession(request.session + ("userName" -> data.userName))
        )
      })
  }
  def logout() = Action.async {
    Future.successful(Redirect(routes.Application.index()).withNewSession)
  }

  def registration() = Action { implicit request =>
    Ok(views.html.registration(UserForm.form))
  }

  def addUser() = Action.async { implicit request =>
    UserForm.form.bindFromRequest.fold(
      errorForm => Future.successful(Ok("User could not be created")),
      data => {
        val newUser = User(0, data.firstName, data.lastName, data.userName, data.email, data.password)
        //should be:
        //userService.addUser(newUser).map(res =>
          //Redirect(routes.Application.index()))
        //For testing
        userService.listAllUsers map { users =>
          Ok(views.html.index(users))} //WHY no table "users" found??
      }
    )
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
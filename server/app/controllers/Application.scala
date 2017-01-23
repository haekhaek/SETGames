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
  
  def index = Action { implicit request =>
    val loggedIn = request.session.get("userName") //TODO
    //val loggedIn = None
    if (loggedIn != None){
      Ok(views.html.home(loggedIn.get))
    }
    else
      Ok(views.html.login(LoginForm.form, ""))
  }

  def login() = Action.async { implicit request =>
    LoginForm.form.bindFromRequest.fold(
      formWithErrors => Future(BadRequest(views.html.login(formWithErrors, "Please fill out all fields!"))),
      data => {
        val logInUser = userService.checkPassword(data.userName, data.password) //Future[Object[User]]
        //for {
          //u <- logInUser
        //} yield u?

        logInUser.flatMap {
          case Some(u) => {Future.successful(Redirect(routes.Application.index())
                .withSession(request.session + ("userName" -> data.userName)))
            }
          case None => Future.successful(Ok(views.html.login(LoginForm.form, "Sorry, login data is not valid!")))
        }
      })
  }

  def logout() = Action.async {
    Future.successful(Redirect(routes.Application.index()).withNewSession)
  }

  def registration() = Action { implicit request =>
    Ok(views.html.registration(UserForm.form, ""))
  }

  def addUser() = Action.async { implicit request =>
    UserForm.form.bindFromRequest.fold(
      formWithErrors => Future(BadRequest(views.html.registration(formWithErrors, "Please fill in form correctly"))),
      data => {
        val newUser = User(0, data.firstName, data.lastName, data.userName, data.email, data.password)

       val existing = userService.getUser(newUser.userName)
        existing.flatMap {
          case Some(x) => Future.successful(Ok(views.html.registration(UserForm.form, "Sorry, Username already exists. Please try again!")))
          case None => userService.addUser(newUser).map {
            res => Ok(views.html.registration(UserForm.form, res))
          }
        }

        /*userService.addUser(newUser).map{
          res => Ok(views.html.registration(UserForm.form, res))
        }*/
      }
    )
  }

    def deleteUser(id: Long) = Action.async { implicit request =>
      userService.deleteUser(id) map { res =>
        Redirect(routes.Application.index())
      }
    }
}

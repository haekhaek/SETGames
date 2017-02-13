package controllers

import scala.concurrent.Future
import javax.inject.Inject
import play.api.mvc.Security.AuthenticatedRequest
import play.api.mvc._
import model.{LoginForm}
import play.api.i18n.{I18nSupport, MessagesApi}


class AuthAction @Inject()(val messagesApi: MessagesApi) extends Controller with I18nSupport{

  object AuthenticatedAction extends ActionBuilder[Request] {
    def invokeBlock[A](request: Request[A], block: (Request[A]) => Future[Result]) = {
      request.session.get("userName") match {
        case Some(userName) => block(new AuthenticatedRequest(userName, request))
        case None => Future.successful(Ok(views.html.login(LoginForm.form, "")))
      }
    }
  }

}
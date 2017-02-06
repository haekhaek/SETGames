package controllers

import play.api._
import play.api.Logger
import play.api.mvc._
import play.api.mvc.Results._
import play.api.Play.current
import scala.concurrent.Future
import service.UserService

/**
  * Created by Eugen Bopp on 23/01/2017.
  */

//object AuthenticatedAction extends ActionBuilder[AuthenticatedRequest] {
//
//  def invokeBlock[A](request: Request[A], block: AuthenticatedRequest[A] => Future[Result]): Future[Result] = {
//
//    request.session.get("username")
//      .flatMap(UserService.findByUsername(_))
//      .map(user => block(new AuthenticatedRequest(user, request)))
//      .getOrElse(Future.successful(Results.Forbidden))
//  }
//}
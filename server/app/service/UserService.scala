package service

/**
  * Created by Eugen Bopp on 15/01/2017.
  */
import model.{User, Users}
import scala.concurrent.Future
import javax.inject.{Singleton, Inject}

@Singleton class UserService @Inject() (val users: Users){

  def addUser(user: User): Future[String] = {
    users.add(user)
  }

  def deleteUser(id: Long): Future[Int] = {
    users.delete(id)
  }

  def getUser(userName: String): Future[Option[User]] = {
    users.getByName(userName)
  }

  def listAllUsers: Future[Seq[User]] = {
    users.listAll
  }


  def checkPassword(userName: String, checkPassword: String): Future[Option[User]]  = {
    users.checkPassword(userName, checkPassword)
  }
}
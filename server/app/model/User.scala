/**
  * Created by Eugen Bopp on 15/01/2017.
  */
package model

import javax.inject.{Inject, Singleton}

import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile
import slick.driver.SQLiteDriver.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class User(id: Long, firstName: String, lastName: String, userName:String, email: String, password: String)
case class UserData(firstName: String, lastName: String, userName: String, email: String, password:String)

object UserForm {
  val form = Form(
    mapping(
      "firstName" -> nonEmptyText,
      "lastName" -> nonEmptyText,
      "userName" -> nonEmptyText,
      "email" -> email,
      "password" -> nonEmptyText
    )(UserData.apply)(UserData.unapply)
  )
}

class UserTableDef(tag: Tag) extends Table[User](tag, "user") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def firstName = column[String]("first_name")
  def lastName = column[String]("last_name")
  def userName = column[String]("user_name")
  def email = column[String]("email")
  def password = column[String]("password")

  override def * =
    (id, firstName, lastName, userName, email, password) <>(User.tupled, User.unapply)
}

@Singleton class Users @Inject() (protected val dbConfigProvider: DatabaseConfigProvider){

  val dbConfig = dbConfigProvider.get[JdbcProfile]
  import dbConfig.driver.api._
//  val dbConfig = DatabaseConfigProvider.get[JdbcProfile](Play.current)

  val users = TableQuery[UserTableDef]

  def add(user: User): Future[String] = {
    dbConfig.db.run(users += user).map(res => "User successfully added").recover {
      case ex: Exception => ex.getCause.getMessage
    }
  }

  def delete(id: Long): Future[Int] = {
    dbConfig.db.run(users.filter(_.id === id).delete)
  }

  def get(id: Long): Future[Option[User]] = {
    dbConfig.db.run(users.filter(_.id === id).result.headOption)
  }

  def listAll: Future[Seq[User]] = {
    dbConfig.db.run(users.result)
  }

}

//for login
case class LoginData(userName: String, password: String)

object LoginForm {
  val form = Form(
    mapping(
      "userName" -> nonEmptyText,
      "password" -> nonEmptyText
    )(LoginData.apply)(LoginData.unapply)
  )
}
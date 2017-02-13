package model
import EloScore._
import javax.inject.{Inject, Singleton}

import play.api.data.Form
import play.api.data.Forms._
import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile
import slick.driver.SQLiteDriver.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._


case class User(id: Long, firstName: String, lastName: String, userName:String, email: String, password: String, score: Int)
case class UserData(firstName: String, lastName: String, userName: String, email: String, password:String)

object UserForm {
  val form = Form(
    mapping(
      "Vorname" -> nonEmptyText,
      "Nachname" -> nonEmptyText,
      "Username" -> nonEmptyText,
      "Email" -> email,
      "Passwort" -> nonEmptyText
    )(UserData.apply)(UserData.unapply)
  )
}

class UserTableDef(tag: Tag) extends Table[User](tag, "users") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def firstName = column[String]("first_name")
  def lastName = column[String]("last_name")
  def userName = column[String]("user_name")
  def email = column[String]("email")
  def password = column[String]("password")
  def score = column[Int]("score", O.Default(1000))

  override def * =
    (id, firstName, lastName, userName, email, password, score) <>(User.tupled, User.unapply)
}

@Singleton class Users @Inject() (protected val dbConfigProvider: DatabaseConfigProvider){

  val dbConfig = dbConfigProvider.get[JdbcProfile]
  val db = dbConfig.db
  import dbConfig.driver.api._
//  val dbConfig = DatabaseConfigProvider.get[JdbcProfile](Play.current)

  val users = TableQuery[UserTableDef]

  def add(user: User): Future[String] = {
    db.run(users += user).map(res => "Registration successfull!")//.recover {
      //case ex: Exception => "Username already exists, please try again!" //can also be different reason?!
    //}
  }

  def delete(id: Long): Future[Int] = {
    db.run(users.filter(_.id === id).delete)
  }

  def get(id: Long): Future[Option[User]] = {
    db.run(users.filter(_.id === id).result.headOption)
  }

  def getByName(userName: String):  Future[Option[User]] = {
   db.run(users.filter(_.userName === userName).result.headOption)
  }

  def checkPassword(userName: String, checkPassword: String): Future[Option[User]] = {
    db.run(users.filter(_.userName === userName).filter(_.password === checkPassword).result.headOption)
  }


  def listAll(sortByScores: Boolean = false): Future[Seq[User]] = {
    if (sortByScores){
      db.run(users.sortBy(_.score.desc).result)
    }
    else{
      db.run(users.sortBy(_.userName.asc).result)
    }
  }

  def updateEloScore(userName: String, outCome: String, competitorName: String){
    val scoreAQuery = users.filter(_.userName === userName).map(_.score)
    val scoreBQuery = users.filter(_.userName === competitorName).map(_.score)

    val scoreA: Future[Int] = db.run(scoreAQuery.result.head)
    val scoreB: Future[Int] = db.run(scoreBQuery.result.head)

    val scA: Int = Await.result(scoreA, 1.second)
    val scB: Int = Await.result(scoreB, 1.second)

    scoreA.onSuccess { case scoreA =>
      scoreB.onSuccess {
        case scoreB => {
          db.run(scoreAQuery.update(calcEloScore(scoreA, scoreB, outCome)))
          db.run(scoreBQuery.update(calcEloScore(scoreB, scoreA, outCome)))
        }
      }
    } //calculate new EloScore

    // val newScoreA: Int = calcEloScore(scA, scB, outCome)
    // db.run(scoreAQuery.update(newScoreA))
  }
}

//for login
case class LoginData(userName: String, password: String)

object LoginForm {
  val form = Form(
    mapping(
      "Username" -> nonEmptyText,
      "Passwort" -> nonEmptyText
    )(LoginData.apply)(LoginData.unapply)
  )
}
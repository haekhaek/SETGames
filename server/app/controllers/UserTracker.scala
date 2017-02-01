package controllers

import shared.GameWrapper
import akka.actor._
import shared.WebSocketMessage
import scala.collection.mutable.{Map, HashMap}
import prickle.Pickle

case class UserRecord(channel : ActorRef, lastActivityTimestamp : Long, var game : Option[GameWrapper])

/**
 * Global singleton for tracking all currently active users.
 */
object UserTracker {

    // Currently hardcoded to expire after 30 minutes of inactivity
    val sessionExpirationLimit : Long = 1000*60*30
    
    val users : Map[String, UserRecord] = new HashMap[String, UserRecord]

    def expired(userId : String) : Boolean = (for {
        record <- users.get(userId)
    } yield (System.currentTimeMillis - record.lastActivityTimestamp > sessionExpirationLimit))
        .getOrElse(false)
    
    def currentlyActiveUsers : Map[String, UserRecord] = users.synchronized {
        users.foreach({case (k, v) => {
            if(expired(k)) {
                users.remove(k)
            }
        }})
        users
    }
    
    def update(userId : String, channel : ActorRef) = users.synchronized {
        val timeStamp = System.currentTimeMillis
        val game = users.get(userId).flatMap(u => u.game)
        users.put(userId, UserRecord(channel, timeStamp, game))
        publishMemberList(game)
    }
    
    def updateGame(userId : String, game : Option[GameWrapper]) = users.synchronized {
        users.get(userId) match {
            case Some(r) => users.put(userId, UserRecord(r.channel, r.lastActivityTimestamp, game))
            case None => users.put(userId, UserRecord(null, System.currentTimeMillis, game))
        }
    }
    
    def remove(userId : String) = users.synchronized {
        for {
            record <- users.get(userId)
        } yield {
            publishMemberList(record.game)
            users.remove(userId)
        }
    }
    
    def publishMemberList(game : Option[GameWrapper]) = {
        val userList = Pickle.intoString(users.keys)
        val messageType = WebSocketMessage.USER_UPDATE.id
        broadcastTo(WebSocketMessage.stringify(WebSocketMessage(messageType,"","all",userList)), game)
    }

    def broadcast(message : String) {
        users.foreach({case (k, v) => {
            v.channel ! message
        }})
    }
    
    def broadcastTo(message : String, game : Option[GameWrapper]) {
        users.filter({case (u,r) => (for {
            g1 <- r.game
            g2 <- game
        } yield {
            g1.getClass.equals(g2.getClass)
        }).getOrElse(false)}).foreach({case (u, r) => {
            r.channel ! message
        }})
    }

    def sendTo(userId : String, message : String) {
        for {
            record <- users.get(userId)
        } yield (record.channel ! message)
    }
    
}

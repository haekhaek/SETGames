package controllers

import shared.GameWrapper
import akka.actor._
import shared.WebSocketMessage
import scala.collection.concurrent
import prickle.Pickle

case class UserRecord(channel : ActorRef, lastActivityTimestamp : Long, var game : Option[GameWrapper])

/**
 * Global singleton for tracking all currently active users.
 */
object UserTracker {

    // Currently hardcoded to expire after 30 minutes of inactivity
    val sessionExpirationLimit : Long = 1000*60*30
    
    val users : concurrent.Map[String, UserRecord] = new concurrent.TrieMap

    def expired(userId : String) : Boolean = users
        .get(userId)
        .map(r => System.currentTimeMillis - r.lastActivityTimestamp > sessionExpirationLimit)
        .getOrElse(false)
    
    def currentlyActiveUsers : concurrent.Map[String, UserRecord] = {
        val expiredUserIDs = users.keys.filter(expired(_))
        for(userId <- expiredUserIDs) {
            users.remove(userId)
        }
        users
    }

    def update(userId : String, channel : ActorRef) = {
        val timeStamp = System.currentTimeMillis
        val game = users.get(userId).flatMap(u => u.game)
        users.put(userId, UserRecord(channel, timeStamp, game))
        publishMemberList(game)
    }
    
    def updateGame(userId : String, game : Option[GameWrapper], channel : ActorRef = null) =
        users.get(userId) match {
            case Some(r) => {
                users.put(userId, UserRecord(r.channel, r.lastActivityTimestamp, game))
                publishMemberList(r.game)
            }
            case None => users.put(userId, UserRecord(channel, System.currentTimeMillis, game))
        }
    
    def remove(userId : String) = 
        for {
            record <- users.get(userId)
        } yield {
            publishMemberList(record.game)
            users.remove(userId)
        }
    
    def publishMemberList(game : Option[GameWrapper]) = {
        val userList = Pickle.intoString(filteredUsersByGame(game).keys)
        val messageType = WebSocketMessage.USER_UPDATE.id
        broadcastTo(WebSocketMessage.stringify(
            WebSocketMessage(messageType,"","all",userList)), game)
    }

    def broadcast(message : String) = users.foreach({case (k, v) => v.channel ! message})
    
    def filteredUsersByGame(game : Option[GameWrapper]) = users.filter({case (u,r) => (for {
            g1 <- r.game
            g2 <- game
        } yield {
            g1.getClass.equals(g2.getClass)
        }).getOrElse(false)})
    
    def broadcastTo(message : String, game : Option[GameWrapper]) =
        filteredUsersByGame(game).foreach({case (u, r) => {
            if(r.channel != null) {
                r.channel ! message
            }
        }})

    def sendTo(userId : String, message : String) = users.get(userId).map(r => r.channel ! message)
    
}

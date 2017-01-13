package controllers

import akka.actor._
import scala.collection.mutable.{Map, HashMap}

case class UserRecord(channel : ActorRef, lastActivityTimestamp : Long)

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
        users.put(userId, UserRecord(channel, System.currentTimeMillis))
    }
    
    def remove(userId : String) = users.synchronized {
        users.remove(userId)
    }

    def broadcast(message : String) {
        users.foreach({case (k, v) => {
            v.channel ! message
        }})
    }

    def sendTo(userId : String, message : String) {
        for {
            record <- users.get(userId)
        } yield (record.channel ! message)
    }
    
}

package controllers

import scala.collection.mutable.{Map, HashMap}

/**
 * Global singleton for tracking all currently active users.
 */
object UserTracker {

    // Currently hardcoded to expire after 30 minutes of inactivity
    val sessionExpirationLimit : Long = 1000*60*30
    
    val users : Map[String, Long] = new HashMap[String, Long]

    def expired(userId : String) : Boolean = (for {
        time <- users.get(userId)
    } yield (System.currentTimeMillis - time > sessionExpirationLimit)).getOrElse(false)
    
    def currentlyActiveUsers : Map[String, Long] = users.synchronized {
        users.foreach({case (k, v) => {
            if(expired(k)) {
                users.remove(k)
            }
        }})
        users
    }
    
    def update(userId : String) = users.synchronized {
            users.put(userId, System.currentTimeMillis)
        }
    
}

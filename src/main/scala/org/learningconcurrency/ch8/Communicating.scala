package org.learningconcurrency
package ch8



import akka.actor._
import akka.event.Logging
import akka.util.Timeout
import scala.concurrent.duration._
import akka.pattern.{ask, pipe}
import scala.concurrent.ExecutionContext.Implicits.global



class Pong extends Actor {
  val log = Logging(context.system, this)
  def receive = {
    case "ping" =>
      log.info("Got a ping -- ponging back!")
      sender ! "pong"
      context.stop(self)
  }
}


class Ping extends Actor {
  import akka.pattern.{ask, pipe}
  import scala.concurrent.ExecutionContext.Implicits.global
  def receive = {
    case pongRef: ActorRef =>
      implicit val timeout = Timeout(2 seconds)
      val future = pongRef ? "ping"
      pipe(future) to sender
  }
}


class Master extends Actor {
  val log = Logging(context.system, this)
  val ping = ourSystem.actorOf(Props[Ping], "ping")
  val pong = ourSystem.actorOf(Props[Pong], "pong")
  override def preStart() = ping ! pong
  def receive = {
    case "pong" =>
      log.info("got a pong back!")
      context.stop(self)
  }
}


object CommunicatingAsk extends App {
  ourSystem.actorOf(Props[Master])
  Thread.sleep(1000)
  ourSystem.shutdown()
}






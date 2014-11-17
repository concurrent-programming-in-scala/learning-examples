package org.learningconcurrency
package ch8



import akka.actor._
import akka.event.Logging
import akka.util.Timeout
import akka.pattern.{ask, pipe, gracefulStop}
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util._



class Pongy extends Actor {
  val log = Logging(context.system, this)
  def receive = {
    case "ping" =>
      log.info("Got a ping -- ponging back!")
      sender ! "pong"
      context.stop(self)
  }
  override def postStop() = log.info("pongy going down")
}


class Pingy extends Actor {
  def receive = {
    case pongyRef: ActorRef =>
      implicit val timeout = Timeout(2 seconds)
      val future = pongyRef ? "ping"
      pipe(future) to sender
  }
}


class Master extends Actor {
  val log = Logging(context.system, this)
  val pingy = ourSystem.actorOf(Props[Pingy], "pingy")
  val pongy = ourSystem.actorOf(Props[Pongy], "pongy")
  def receive = {
    case "start" =>
      pingy ! pongy
    case "pong" =>
      log.info("got a pong back!")
      context.stop(self)
  }
  override def postStop() = log.info("master going down")
}


object CommunicatingAsk extends App {
  val masta = ourSystem.actorOf(Props[Master], "masta")
  masta ! "start"
  Thread.sleep(1000)
  ourSystem.shutdown()
}


class Router extends Actor {
  var i = 0
  val children = for (_ <- 0 until 4) yield context.actorOf(Props[StringPrinter])
  def receive = {
    case "stop" => context.stop(self)
    case msg =>
      children(i) forward msg
      i = (i + 1) % 4
  }
}


object CommunicatingRouter extends App {
  val router = ourSystem.actorOf(Props[Router], "router")
  router ! "Hi."
  router ! "I'm talking to you!"
  Thread.sleep(1000)
  router ! "stop"
  Thread.sleep(1000)
  ourSystem.shutdown()
}


object CommunicatingPoisonPill extends App {
  val masta = ourSystem.actorOf(Props[Master], "masta")
  masta ! akka.actor.PoisonPill
  Thread.sleep(1000)
  ourSystem.shutdown()
}


class GracefulPingy extends Actor {
  val pongy = context.actorOf(Props[Pongy], "pongy")
  context.watch(pongy)

  def receive = {
    case GracefulPingy.CustomShutdown =>
      context.stop(pongy)
    case Terminated(`pongy`) =>
      context.stop(self)
  }
}


object GracefulPingy {
  object CustomShutdown
}


object CommunicatingGracefulStop extends App {
  val grace = ourSystem.actorOf(Props[GracefulPingy], "grace")
  val stopped = gracefulStop(grace, 3.seconds, GracefulPingy.CustomShutdown)
  stopped onComplete {
    case Success(x) =>
      log("graceful shutdown successful")
      ourSystem.shutdown()
    case Failure(t) =>
      log("grace not stopped!")
      ourSystem.shutdown()
  }
}








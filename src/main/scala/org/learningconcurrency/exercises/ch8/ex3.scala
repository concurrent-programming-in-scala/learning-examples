package org.learningconcurrency
package exercises
package ch8

import akka.actor.{Props, Actor, ActorSystem, ActorRef}
import akka.actor.Actor.Receive
import org.learningconcurrency.exercises.ch8.Ex3.SessionActor.{EndSession, StartSession}

/**
 * Implement the SessionActor class, for actors that control access to other actors:
 * class SessionActor(password: String, r: ActorRef) extends Actor {
 *  def receive = ???
 *  }
 *  After the SessionActor instance receives the StartSession message with the correct password,
 *  it forwards all the messages to the actor reference r,
 *  until it receives the EndSession message.
 *  Use behaviors to model this actor.
 */
object Ex3 extends App {

  class SessionActor(password: String, r: ActorRef) extends Actor {
    override def receive: Receive = waitStart

    def waitStart:Receive = {
      case StartSession(p) if (p == password) =>
        context.become(receiveMessage)
        log("start session")
      case m => log(s"Can't forward $m. Waiting start session ...")
    }

    def receiveMessage: Receive = {
      case EndSession =>
        context.become(waitStart)
        log("end session")
      case m => r forward m
    }
  }

  object SessionActor {

    def props(password:String, r:ActorRef) = Props(classOf[SessionActor],password,r)

    case class StartSession(password: String)
    case object EndSession
  }


  //test

  class TestActor extends Actor {
    override def receive: Actor.Receive = {
      case m => log(m.toString)
    }
  }

  object TestActor {
    val props = Props[TestActor]
  }

  val system = ActorSystem("Ch3System")

  val testActor = system.actorOf(TestActor.props,"TestActor")
  val sessionActor = system.actorOf(SessionActor.props("123",testActor))

  sessionActor ! "Test1"
  sessionActor ! StartSession("123")
  sessionActor ! "Test2"
  sessionActor ! "Test3"
  sessionActor ! EndSession
  sessionActor ! "Test4"

  Thread.sleep(5000)

  system.shutdown()

}

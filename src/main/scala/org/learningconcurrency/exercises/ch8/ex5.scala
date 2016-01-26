package org.learningconcurrency
package exercises
package ch8

import akka.actor._
import akka.event.Logging
import akka.pattern._

import scala.concurrent.duration._
import scala.util.{Failure, Success}

import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Implement the FailureDetector actor, which sends Identify messages to the specified
  * actors every interval seconds.
  * If an actor does not reply with any ActorIdentity messages within threshold seconds,
  * the FailureDetector actor sends a Failed message to its parent actor,
  * which contains the actor reference of the failed actor.
  */
object Ex5 extends App {

  class FailureDetectorActor(r: ActorRef, interval: Int, threshold: Int) extends Actor {

    val log = Logging(context.system, this)

    val cancelable = context.system.scheduler.schedule(
      initialDelay = 0 seconds, interval = interval seconds, receiver = self, message = FailureDetectorActor.Iteration)

    override def receive: Receive = {
      case FailureDetectorActor.Iteration =>
        log.info("send identify message")
        (r ? Identify(r.path))(threshold seconds) onComplete {
          case Success(_) => log.info("OK")
          case Failure(e) =>
            log.info(s"$r Not reply ")
            context.actorSelection(r.path.parent) ! ParentActor.Failed(r)
        }
      case FailureDetectorActor.StopTask =>
        log.info(s"Stop task")
        cancelable.cancel()
    }
  }

  object FailureDetectorActor {
    case object Iteration
    case object StopTask
    def props(r: ActorRef, interval: Int, threshold: Int) = Props(classOf[FailureDetectorActor], r, interval, threshold)
  }

  //test
  class TestActor extends Actor {
    override def receive: Actor.Receive = PartialFunction.empty
  }

  object TestActor {
    val props = Props[TestActor]
  }

  class ParentActor extends Actor {

    val log = Logging(context.system, this)

    val testActor = context.actorOf(TestActor.props, "TestActor")
    val failureDetectorActor = system.actorOf(FailureDetectorActor.props(testActor, 1, 2), "FailureDetector")

    override def receive: Actor.Receive = {
      case ParentActor.StopTestActor =>
        log.info("stop test actor")
        context.stop(testActor)
      case ParentActor.Failed(r) =>
        log.info(s"Parent. $r not reply !!!!")
        failureDetectorActor ! FailureDetectorActor.StopTask
    }
  }

  object ParentActor {

    case class Failed(r: ActorRef)

    case object StopTestActor

    val props = Props[ParentActor]

  }

  import org.learningconcurrency.exercises.ch8.Ex5.ParentActor._

  val system = ActorSystem("FailureDetectorSystem")
  val parentActor = system.actorOf(ParentActor.props, "ParentActor")

  Thread.sleep(5000)
  parentActor ! StopTestActor
  Thread.sleep(5000)

  system.shutdown()

}

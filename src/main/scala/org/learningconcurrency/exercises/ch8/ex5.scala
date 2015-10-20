package org.learningconcurrency
package exercises
package ch8

import akka.actor._
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

    import org.learningconcurrency.exercises.ch8.Ex5.FailureDetectorActor._
    import org.learningconcurrency.exercises.ch8.Ex5.ParentActor._

    val cancelable = context.system.scheduler.schedule(0 seconds, interval seconds) {
        log("send identify message")
        (r ? Identify(r.path))(threshold seconds) onComplete {
          case Success(_) => log("OK")
          case Failure(e) =>
            log(s"Not reply ")
            log(s"parent ${context.parent}")
            context.actorSelection(r.path.parent) ! Failed(r)
        }
    }

    override def receive: Receive = {
      case StopTask =>
        log(s"Stop task")
        cancelable.cancel()
    }
  }

  object FailureDetectorActor {
    case object StopTask
    def props(r: ActorRef, interval: Int, threshold: Int) = Props(classOf[FailureDetectorActor], r, interval, threshold)
  }

  //test
  class TestActor extends Actor {
    import org.learningconcurrency.exercises.ch8.Ex5.TestActor.Print

    override def receive: Actor.Receive = {
      case Print => log(s"TestActor")
    }
  }

  object TestActor {
    case object Print
    val props = Props[TestActor]
  }

  class ParentActor extends Actor {

    import org.learningconcurrency.exercises.ch8.Ex5.ParentActor._
    import org.learningconcurrency.exercises.ch8.Ex5.FailureDetectorActor._

    val testActor = context.actorOf(TestActor.props, "TestActor")
    val failureDetectorActor = system.actorOf(FailureDetectorActor.props(testActor, 1, 2), "FailureDetector")

    override def receive: Actor.Receive = {
      case StopTestActor =>
        log("stop test actor")
        context.stop(testActor)
      case Failed(r) => failureDetectorActor ! StopTask
    }
  }

  object ParentActor {
    case class Failed(r:ActorRef)
    case object StopTestActor
    val props = Props[ParentActor]

  }

  import org.learningconcurrency.exercises.ch8.Ex5.ParentActor._

  val system = ActorSystem("FailureDetectorSystem")
  val parentActor = system.actorOf(ParentActor.props,"ParentActor")

  Thread.sleep(5000)
  parentActor ! StopTestActor
  Thread.sleep(5000)

  system.shutdown()

}

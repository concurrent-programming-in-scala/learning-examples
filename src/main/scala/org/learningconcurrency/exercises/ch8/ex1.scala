package org.learningconcurrency
package exercises
package ch8

import java.util.TimerTask

import akka.actor._
import akka.pattern.pipe

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

/** Implement the timer actor with the TimerActor class.
  *
  * After receiving a Register message containing the t timeout in milliseconds,
  * the timer actor sends a Timeout message back after t milliseconds.
  *
  * The timer must accept multiple Register messages.
  */
object Ex1 extends App {

  def timerFuture(t: Int): Future[Unit] = {
    val p = Promise[Unit]

    import java.util.Timer
    (new Timer(true)).
      schedule(new TimerTask {
        override def run(): Unit = p.success()
      }, t)

    p.future
  }

  import org.learningconcurrency.exercises.ch8.Ex1.TestActor.Register
  import org.learningconcurrency.exercises.ch8.Ex1.TimerActor.Timeout

  class TimerActor extends Actor {
    override def receive = {
      case Register(t) =>
        log(s"REGISTER MESSAGE (timeout = $t) from $sender")
        timerFuture(t) map { (_) => Timeout } pipeTo sender
    }
  }

  object TimerActor {
    val props = Props[TimerActor]
    case object Timeout
  }

  //test

  class TestActor(t: Int) extends Actor {
    context.actorSelection("/user/timerActor") ! Register(t)

    override def receive: Receive = {
      case Timeout => log(s"TIMEOUT MESSAGE from ${sender}")
    }
  }

  object TestActor {
    case class Register(t: Int)
    def props(t: Int) = Props(classOf[TestActor], t)
  }

  val system = ActorSystem("MyActorSystem")
  val timerActor = system.actorOf(TimerActor.props, "timerActor")

  (1 to 10) map ((i) => system.actorOf(TestActor.props(i * 1000), s"testActor-$i"))

  Thread.sleep(12000)
  system.shutdown()


}

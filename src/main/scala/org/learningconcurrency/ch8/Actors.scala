package org.learningconcurrency
package ch8



import akka.actor.Actor
import akka.actor.ActorRef
import akka.event.Logging
import akka.actor.Props
import akka.actor.ActorSystem



class HelloActor(val hello: String) extends Actor {
  val log = Logging(context.system, this)
  def receive = {
    case `hello` =>
      log.info(s"Received a '$hello'... $hello!")
    case msg     =>
      log.info(s"Unexpected message '$msg'")
      context.stop(self)
  }
}


object HelloActor {
  def props(hello: String) = Props(new HelloActor(hello))
  def propsVariant(hello: String) = Props(classOf[HelloActor], hello)
}


object ActorsCreate extends App {
  val hiActor = ourSystem.actorOf(HelloActor.props("hi"), name = "greeter")
  hiActor ! "hi"
  Thread.sleep(1000)
  hiActor ! "hola"
  Thread.sleep(1000)
  ourSystem.shutdown()
}


class DeafActor extends Actor {
  val log = Logging(context.system, this)
  def receive = PartialFunction.empty
  override def unhandled(msg: Any) = log.info(s"could not handle '$msg'")
}


object ActorsUnhandled extends App {
  val deafActor = ourSystem.actorOf(Props[DeafActor], name = "deafy")
  deafActor ! "hi"
  Thread.sleep(1000)
  deafActor ! "can you hear me?"
  ourSystem.stop(deafActor)
  Thread.sleep(1000)
  ourSystem.shutdown()
}


class ParentActor extends Actor {
  val log = Logging(context.system, this)
  def receive = {
    case "create" =>
      context.actorOf(Props[ChildActor], "kid")
      log.info(s"created a new child - children = ${context.children}")
    case "sayhi" =>
      log.info("Kids, say hi!")
      for (c <- context.children) c ! "sayhi"
    case "stop" =>
      log.info("parent stopping")
      context.stop(self)
  }
}


class ChildActor extends Actor {
  val log = Logging(context.system, this)
  def receive = {
    case "sayhi" => log.info("hello, everybody!")
  }
  override def postStop() {
    log.info("kid got stopped!")
  }
}


object ActorsHierarchy extends App {
  val parent = ourSystem.actorOf(Props[ParentActor], "parent")
  parent ! "create"
  Thread.sleep(1000)
  parent ! "sayhi"
  Thread.sleep(1000)
  parent ! "stop"
  Thread.sleep(1000)
  ourSystem.shutdown()
}


class ExampleActor extends Actor {
  val log = Logging(context.system, this)
  var child: ActorRef = _
  def receive = {
    case num: Double  => log.info(s"got a double - $num")
    case num: Int     => log.info(s"got an integer - $num")
    case lst: List[_] => log.info(s"list - ${lst.head}, ...")
    case txt: String  => child ! txt
  }
  override def preStart(): Unit = {
    log.info("about to start")
    child = context.actorOf(Props[ExampleChildActor], "kid")
  }
  override def preRestart(reason: Throwable, msg: Option[Any]): Unit = {
    log.info(s"about to restart because of $reason")
    super.preRestart(reason, msg)
  }
  override def postRestart(reason: Throwable): Unit = {
    log.info(s"just restarted due to $reason")
    super.postRestart(reason)
  }
  override def postStop() = log.info("just stopped")
}


class ExampleChildActor extends Actor {
  val log = Logging(context.system, this)
  def receive = {
    case msg => log.info(s"child got message '$msg'")
  }
  override def preStart(): Unit = log.info(s"child about to start.")
  override def postStop(): Unit = log.info(s"child just stopped.")
}


object ActorsLifecycle extends App {
  val example = ourSystem.actorOf(Props[ExampleActor], "example")
  example ! math.Pi
  Thread.sleep(1000)
  example ! 7
  Thread.sleep(1000)
  example ! "hi there!"
  Thread.sleep(1000)
  example ! Nil
  Thread.sleep(1000)
  example ! "sorry about that"
  Thread.sleep(1000)
  ourSystem.stop(example)
  Thread.sleep(1000)
  ourSystem.shutdown()
}
















package org.learningconcurrency
package ch8



import akka.actor._
import akka.event.Logging
import akka.actor.SupervisorStrategy._
import org.apache.commons.io.FileUtils
import scala.io.Source
import scala.collection._
import scala.concurrent.duration._



class Victim extends Actor {
  val log = Logging(context.system, this)
  def receive = {
    case msg => log.info(s"got a message '$msg'")
  }
}


class Supervisor extends Actor {
  val child = context.actorOf(Props[Victim], "victim")
  def receive = {
    case "stop" => context.stop(self)
  }
  override val supervisorStrategy =
    OneForOneStrategy() {
      case ake: ActorKilledException => Restart
      case _ => Escalate
    }
}


object SupervisionKill extends App {
  val s = ourSystem.actorOf(Props[Supervisor], "super")
  Thread.sleep(1000)
  ourSystem.actorSelection("/user/super/*") ! Kill
  ourSystem.actorSelection("/user/super/*") ! "sorry about that"
  Thread.sleep(1000)
  s ! "stop"
  Thread.sleep(1000)
  ourSystem.shutdown()
}


class Downloader extends Actor {
  def receive = {
    case DownloadManager.Download(url, dest) =>
      val content = Source.fromURL(url)
      FileUtils.write(new java.io.File(dest), content.mkString)
      sender ! DownloadManager.Finished(dest)
  }
}


class DownloadManager(val downloadSlots: Int) extends Actor {
  val log = Logging(context.system, this)
  val downloaders = mutable.Queue[ActorRef]()
  val pendingWork = mutable.Queue[DownloadManager.Download]()
  val workItems = mutable.Map[ActorRef, DownloadManager.Download]()
  
  override def preStart(): Unit = {
    for (i <- 0 until downloadSlots) downloaders.enqueue(context.actorOf(Props[Downloader], s"dl$i"))
  }
  
  private def checkMoreDownloads(): Unit = {
    if (pendingWork.nonEmpty && downloaders.nonEmpty) {
      val dl = downloaders.dequeue()
      val workItem = pendingWork.dequeue()
      log.info(s"$workItem starting, ${downloaders.size} download slots left")
      dl ! workItem
      workItems(dl) = workItem
    }
  }

  def receive = {
    case msg @ DownloadManager.Download(url, dest) =>
      pendingWork.enqueue(msg)
      checkMoreDownloads()
    case DownloadManager.Finished(dest) =>
      log.info(s"Download to '$dest' finished, ${downloaders.size} download slots left")
      workItems.remove(sender)
      downloaders.enqueue(sender)
      checkMoreDownloads()
  }

  override val supervisorStrategy = 
    OneForOneStrategy(maxNrOfRetries = 6, withinTimeRange = 30 seconds) {
      case fnf: java.io.FileNotFoundException =>
        log.info(s"Resource could not be found: $fnf")
        workItems.remove(sender)
        downloaders.enqueue(sender)
        Resume
      case _ =>
        Escalate
    }
}


object DownloadManager {
  case class Download(url: String, dest: String)
  case class Finished(dest: String)
  case class ResourceNotFound(url: String) extends Exception
  case class DestinationNotWriteable(filename: String) extends Exception
}


object SupervisionDownloader extends App {
  import DownloadManager._
  val downloadManager = ourSystem.actorOf(Props(classOf[DownloadManager], 4), "manager")
  downloadManager ! Download("http://www.w3.org/Addressing/URL/url-spec.txt", "url-spec.txt")
  downloadManager ! Download("http://www.scala-lang.org/misc/blues.mp3", "TheScalaBlues.mp3")
  Thread.sleep(1000)
  downloadManager ! Download("https://github.com/scala/scala/blob/master/README.md", "SCALA-README.md")
  Thread.sleep(5000)
  ourSystem.stop(downloadManager)
  Thread.sleep(1000)
  ourSystem.shutdown()
}










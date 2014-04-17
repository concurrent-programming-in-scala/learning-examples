package org.learningconcurrency
package ch6






object ObservablesItems extends App {
  import rx.lang.scala._

  val o = Observable.items("Pascal", "Java", "Scala")
  o.subscribe(name => log(s"learned the $name language"))
  o.subscribe(name => log(s"forgot the $name language"))

}


object ObservablesLifetime extends App {
  import rx.lang.scala._

  val classics = List("Il buono, il brutto, il cattivo.", "Back to the future", "Die Hard")
  val o = Observable.from(classics)

  o.subscribe(
    m => log(s"Movies Watchlist - $m"),
    e => log(s"Ooops - $e!"),
    () => log(s"No more movies.")
  )

}


object ObservablesCombinators extends App {
  import rx.lang.scala._

  val roles = Observable.items("The Good", "The Bad", "The Ugly")
  val names = Observable.items("Clint Eastwood", "Lee Van Cleef", "Eli Wallach")
  val zipped = names.zip(roles).map { case (name, role) => s"$name - $role" }

  zipped.subscribe(log _)

}


object ObservablesCreate extends App {
  import rx.lang.scala._

  val vms = Observable[String] { subscriber =>
    subscriber.onNext("JVM")
    subscriber.onNext(".NET")
    subscriber.onNext("DartVM")
    subscriber.onCompleted()
  }

  log(s"About to subscribe")
  vms.subscribe(log _, e => log(s"oops - $e"), () => log("Done!"))
  log(s"Subscription returned")

}


object ObservablesCreateAsynchronous extends App {
  import rx.lang.scala._
  import org.apache.commons.io.monitor._
  import java.io.File

  def modifiedFiles(directory: String): Observable[String] = {
    Observable { subscriber =>
      val fileMonitor = new FileAlterationMonitor(1000)
      val fileObs = new FileAlterationObserver(directory)
      val fileLis = new FileAlterationListenerAdaptor {
        override def onFileChange(file: File) {
          subscriber.onNext(file.getName)
        }
      }
      fileObs.addListener(fileLis)
      fileMonitor.addObserver(fileObs)
      fileMonitor.start()

      subscriber.add(Subscription { fileMonitor.stop() })
    }
  }

  log(s"adding subscription")
  val subscription = modifiedFiles(".").subscribe(filename => log(s"$filename modified!"))
  log(s"subscription added")

  Thread.sleep(10000)

  log(s"unsubscribing and exiting")
  subscription.unsubscribe()

}


object ObservablesHotVsCold extends App {
  import java.util.{Timer, TimerTask}
  import scala.collection._
  import rx.lang.scala._

  val songs = List("Eye of the Tiger", "You Spin Me Round", "Rebel Yell")
  val myPlaylist = Observable.from(songs)

  object Player extends TimerTask {
    val timer = new Timer
    var index = 0
    var subscribers = mutable.Set[Subscriber[String]]()
    timer.schedule(this, 0L, 1000L)

    def run() {
      index = (index + 1) % songs.length
      Player.synchronized { for (s <- subscribers) s.onNext(songs(index)) }
    }
    def add(s: Subscriber[String]) = Player.synchronized { subscribers += s }
    def remove(s: Subscriber[String]) = Player.synchronized { subscribers -= s }
  }
  val currentlyPlaying = Observable[String] { subscriber =>
    Player.add(subscriber)
    subscriber.add(Subscription { Player.remove(subscriber) })
  }

  log(s"adding to a cold observable")
  myPlaylist.subscribe(log _)
  log(s"adding to a cold observable again")
  myPlaylist.subscribe(log _)
  Thread.sleep(2000)

  log(s"adding to a hot observable")
  val subscription1 = currentlyPlaying.subscribe(log _)
  Thread.sleep(2400)
  subscription1.unsubscribe()
  Thread.sleep(1200)
  log(s"adding to a hot observable again")
  val subscription2 = currentlyPlaying.subscribe(log _)
  Thread.sleep(2400)
  subscription2.unsubscribe()
  Thread.sleep(1200)
  log(s"Done -- shutting down the Player")
  Player.timer.cancel()

}











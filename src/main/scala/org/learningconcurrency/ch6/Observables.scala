package org.learningconcurrency
package ch6






object ObservablesItems extends App {
  import rx.lang.scala._

  val o = Observable.items("Pascal", "Java", "Scala")
  o.subscribe(name => log(s"learned the $name language"))
  o.subscribe(name => log(s"forgot the $name language"))

}


object ObservablesTimer extends App {
  import rx.lang.scala._
  import scala.concurrent.duration._

  val o = Observable.timer(1 second)
  o.subscribe(_ => log(s"Timeout!"))
  o.subscribe(_ => log(s"Another timeout!"))
}


object ObservablesExceptions extends App {
  import rx.lang.scala._

  val o = Observable.items(1, 2) ++ Observable.error(new RuntimeException) ++ Observable.items(3, 4)
  o.subscribe(
    x => log(s"number $x"),
    t => log(s"an error occurred: $t")
  )
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


object ObservablesFromFuture extends App {
  import rx.lang.scala._
  import scala.concurrent._
  import ExecutionContext.Implicits.global

  val o = Observable.from(Future {
    Thread.sleep(500)
    "Back to the Future(s)"
  })

  o.subscribe(log _)
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

  log(s"starting to monitor files")
  val subscription = modifiedFiles(".").subscribe(filename => log(s"$filename modified!"))
  log(s"please modify and save a file")

  Thread.sleep(10000)

  subscription.unsubscribe()
  log(s"monitoring done")

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
    def start() = timer.schedule(this, 0L, 1000L)
    def stop() = timer.cancel()

    def run() {
      index = (index + 1) % songs.length
      Player.synchronized { for (s <- subscribers) s.onNext(songs(index)) }
    }
    def turnOn(s: Subscriber[String]) = Player.synchronized { subscribers += s }
    def turnOff(s: Subscriber[String]) = Player.synchronized { subscribers -= s }
  }
  Player.start()

  val currentlyPlaying = Observable[String] { subscriber =>
    Player.turnOn(subscriber)
    subscriber.add(Subscription { Player.turnOff(subscriber) })
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
  Player.stop()

}











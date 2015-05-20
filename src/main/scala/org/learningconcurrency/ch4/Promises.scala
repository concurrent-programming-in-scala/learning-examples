package org.learningconcurrency
package ch4






object PromisesCreate extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global

  val p = Promise[String]
  val q = Promise[String]

  p.future foreach {
    case text => log(s"Promise p succeeded with '$text'")
  }

  p success "kept"
  
  val secondAttempt = p trySuccess "kept again"

  log(s"Second attempt to complete the same promise went well? $secondAttempt")

  q failure new Exception("not kept")

  q.future.failed foreach {
    case t => log(s"Promise q failed with $t")
  }

}


object PromisesCustomAsync extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.util.control.NonFatal

  def myFuture[T](body: =>T): Future[T] = {
    val p = Promise[T]

    global.execute(new Runnable {
      def run() = try {
        val result = body
        p success result
      } catch {
        case NonFatal(e) =>
          p failure e
      }
    })

    p.future
  }

  val future = myFuture {
    "naaa" + "na" * 8 + " Katamari Damacy!"
  }

  future foreach {
    case text => log(text)
  }

}


object PromisesAndCallbacks extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import org.apache.commons.io.monitor._
  import java.io.File

  def fileCreated(directory: String): Future[String] = {
    val p = Promise[String]

    val fileMonitor = new FileAlterationMonitor(1000)
    val observer = new FileAlterationObserver(directory)
    val listener = new FileAlterationListenerAdaptor {
      override def onFileCreate(file: File) {
        try p.trySuccess(file.getName)
        finally fileMonitor.stop()
      }
    }
    observer.addListener(listener)
    fileMonitor.addObserver(observer)
    fileMonitor.start()

    p.future
  }

  fileCreated(".") foreach {
    case filename => log(s"Detected new file '$filename'")
  }

}


object PromisesAndCustomOperations extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global

  implicit class FutureOps[T](val self: Future[T]) {
    def or(that: Future[T]): Future[T] = {
      val p = Promise[T]
      self onComplete { case x => p tryComplete x }
      that onComplete { case y => p tryComplete y }
      p.future
    }
  }

  val f = Future { "now" } or Future { "later" }

  f foreach {
    case when => log(s"The future is $when")
  }

}


object PromisesAndTimers extends App {
  import java.util._
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import PromisesAndCustomOperations._

  private val timer = new Timer(true)

  def timeout(millis: Long): Future[Unit] = {
    val p = Promise[Unit]
    timer.schedule(new TimerTask {
      def run() = p.success(())
    }, millis)
    p.future
  }

  val f = timeout(1000).map(_ => "timeout!") or Future {
    Thread.sleep(999)
    "work completed!"
  }

  f foreach {
    case text => log(text)
  }

}


object PromisesCancellation extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global

  def cancellable[T](b: Future[Unit] => T): (Promise[Unit], Future[T]) = {
    val p = Promise[Unit]
    val f = Future {
      val r = b(p.future)
      if (!p.tryFailure(new Exception))
        throw new CancellationException
      r
    }
    (p, f)
  }

  val (cancel, value) = cancellable { cancel =>
    var i = 0
    while (i < 5) {
      if (cancel.isCompleted) throw new CancellationException
      Thread.sleep(500)
      log(s"$i: working")
      i += 1
    }
    "resulting value"
  }

  Thread.sleep(1500)

  cancel.trySuccess(())

  log("computation cancelled!")
}



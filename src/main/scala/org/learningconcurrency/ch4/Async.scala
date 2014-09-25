package org.learningconcurrency
package ch4






object AsyncBasic extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.async.Async.{async, await}

  val workerName: Future[String] = async {
    Thread.currentThread.getName
  }

  workerName foreach {
    name => log(s"Future completed by worker $name")
  }

}


object AsyncAwait extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.async.Async.{async, await}
  import scala.io.Source

  val timetableFuture: Future[String] = async {
    val utc: Future[String] = async { Source.fromURL("http://www.timeapi.org/utc/now").mkString }
    val pdt: Future[String] = async { Source.fromURL("http://www.timeapi.org/pdt/now").mkString }
    val wet: Future[String] = async { Source.fromURL("http://www.timeapi.org/west/now").mkString }
    s"""Timetable
    Universal Time                 ${await { utc } }
    Pacific Daylight Time          ${await { pdt } }
    Western European Summer Time   ${await { wet } }
    """
  }

  timetableFuture foreach {
    timetable => log(timetable)
  }

}


object AsyncWhile extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.async.Async.{async, await}

  def delay(nSeconds: Int) = async {
    blocking {
      Thread.sleep(nSeconds * 1000)
    }
  }

  def simpleCount(): Future[Unit] = async {
    log("T-minus 2 seconds")
    await { delay(1) }
    log("T-minus 1 second")
    await { delay(1) }
    log("done!")
  }

  simpleCount()

  Thread.sleep(3000)

  def countdown(nSeconds: Int)(count: Int => Unit): Future[Unit] = async {
    var i = nSeconds
    while (i > 0) {
      count(i)
      await { delay(1) }
      i -= 1
    }
  }

  countdown(10) { n =>
    log(s"T-minus $n seconds")
  } foreach {
    _ => log(s"This program is over!")
  }

}


package org.learningconcurrency
package ch4






object BlockingAwait extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  import scala.io.Source

  val urlSpecSizeFuture = Future { Source.fromURL("http://www.w3.org/Addressing/URL/url-spec.txt").size }
  val urlSpecSize = Await.result(urlSpecSizeFuture, 10.seconds)

  log(s"url spec contains $urlSpecSize characters")

}


object BlockingSleepBad extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  val startTime = System.nanoTime

  val futures = for (_ <- 0 until 16) yield Future {
    Thread.sleep(1000)
  }

  for (f <- futures) Await.ready(f, Duration.Inf)

  val endTime = System.nanoTime

  log(s"Total execution time of the program = ${(endTime - startTime) / 1000000} ms")
  log(s"Note: there are ${Runtime.getRuntime.availableProcessors} CPUs on this machine")

}


object BlockingSleepOk extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  val startTime = System.nanoTime

  val futures = for (_ <- 0 until 16) yield Future {
    blocking {
      Thread.sleep(1000)
    }
  }

  for (f <- futures) Await.ready(f, Duration.Inf)

  val endTime = System.nanoTime

  log(s"Total execution time of the program = ${(endTime - startTime) / 1000000} ms")

}


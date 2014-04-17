package org.learningconcurrency
package ch6






object SchedulersUntil extends App {
  import rx.lang.scala._
  import scala.concurrent.duration._

  val working = Observable.interval(0.5 seconds).map(_ => "working ...")
  def timeout(d: Duration) = Observable.interval(d).take(1)

  (working.takeUntil(timeout(4 seconds)) ++ Observable.items("stopped")).subscribe(log _)

}



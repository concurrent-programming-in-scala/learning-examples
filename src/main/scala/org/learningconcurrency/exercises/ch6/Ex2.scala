package org.learningconcurrency
package exercises
package ch6

/**
  * Implement an Observable object that emits an event every 5 seconds and every 12 seconds,
  * but not if the elapsed time is a multiple of 30 seconds.
  * Use functional combinators on Observable objects.
  */

import rx.lang.scala.Observable
import scala.concurrent.duration._

object Ex2A extends App {

  val a = Observable.interval(5 seconds).map(_ * 5)
  val b = Observable.interval(12 seconds).map(_ * 12)

  val c = (a merge b distinct) filter (_ % 30 != 0)

  c.subscribe((s) => log(s.toString))

  Thread.sleep(70000)
}

object Ex2B extends App {

  val d = Observable.interval(1 seconds).filter((l) => (l % 30 != 0) && ((l % 5 == 0) || (l % 12 == 0)))
  d.subscribe((s) => log(s.toString))

  Thread.sleep(70000)

}

package org.learningconcurrency
package exercises
package ch6

import rx.lang.scala._

import org.learningconcurrency.ch2._

/**
  * Implement a custom Observable[Thread] object that emits an event when it detects that a thread was started.
  * The implementation is allowed to miss some of the events.
  */
object Ex1 extends App {

  val observable = Observable.apply[Thread] {
    obs =>

      val t = new Thread {
        override def start(): Unit = {
          super.start()
          obs.onNext(this)
        }
      }

      obs.onNext(t)
      Subscription()
  }

  observable.subscribe((t) => log(t.toString()))

}

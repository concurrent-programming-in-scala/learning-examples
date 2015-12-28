package org.learningconcurrency
package exercises
package ch6

/**
  Implement the reactive signal abstraction, represented with the Signal[T] type.

  The Signal[T] type comes with the method apply, used to query the last event emitted by this signal,
  and several combinators with the same semantics as the corresponding Observable methods:

  class Signal[T] {
     def apply(): T = ???
     def map(f: T => S): Signal[S] = ???
     def zip[S](that: Signal[S]): Signal[(T, S)] = ???
     def scan[S](z: S)(f: (S, T) => S) = ???
  }

  Then, add the method toSignal to the Observable[T] type, which converts
  an Observable object to a reactive signal: def toSignal: Signal[T] = ???

  Consider using Rx subjects for this task.
  */

import rx.lang.scala._

object Ex4 extends App {

  implicit class ObserverableAdditional[T](val self:Observable[T]) extends AnyVal {

    def toSignal:Signal[T] = {
      new Signal[T](self)
    }

  }

  class Signal[T] {

    var lastEvent:T = _

    var observable: Observable[T] = _

    val subject = Subject[T]()
    subject.subscribe(lastEvent = _)

    def this(observable: Observable[T]) = {
      this()

      this.observable = observable.last
      this.observable.subscribe(subject)
    }

    def apply(): T = lastEvent

    def  map[S](f: T => S): Signal[S] =
      this.observable.map(f).toSignal

    def zip[S](that: Signal[S]): Signal[(T, S)] =
      this.observable.zip(that.observable).toSignal

    def scan[S](z: S)(f: (S, T) => S):Signal[S] =
      this.observable.scan(z)(f).toSignal

  }

  //test
  def test = {

    val o = Observable.from(List(1,2,3,4,5))
    val o2 = Observable.from(List("A","B","C","D","E"))

    val s1 = o.toSignal
    val s2 =o2.toSignal

    log(s"s1: element = ${s1()}")
    log(s"s2: element = ${s2()}")

    val sMap = s1.map(_+"~")
    log(s"sMap: element = ${sMap()}")

    val sZip = s1.zip(s2)
    log(s"sZip: element = ${sZip()}")

    val sScan = s1.scan(2)((s,t)=>s*t)
    log(s"sScan: element = ${sScan()}")
  }

  test
  Thread.sleep(5000)

}

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
    protected var lastEvent: Option[T] = None
    protected var observable: Observable[T] = _

    def this(observable: Observable[T]) {
      this()
      setObservable(observable)
    }

    def this(observable: Observable[T], initial: T) {
      this(observable)
      lastEvent = Option(initial)
    }

    protected def setObservable(observable: Observable[T]): Unit = {
      this.observable = observable
      this.observable.subscribe(t => { lastEvent = Option(t) })
    }

    /* This method throws `NoSuchElementException` when any of events are not emitted. */
    def apply(): T = lastEvent.get

    def map[S](f: T => S): Signal[S] = lastEvent match {
      case Some(t) => new Signal(observable.map(f), f(t))
      case _ => new Signal(observable.map(f))
    }

    def zip[S](that: Signal[S]): Signal[(T, S)] = (lastEvent, that.lastEvent) match {
      case (Some(t), Some(s)) => new Signal(observable.zip(that.observable), (t, s))
      case (_, _) => new Signal(observable.zip(that.observable))
    }

    def scan[S](z: S)(f: (S, T) => S): Signal[S] =
      new Signal(observable.scan(z)(f))
  }

  val sub1 = Subject[Int]()
  val sig1 = sub1.toSignal
  sub1.onNext(1)
  assert(sig1() == 1)
  sub1.onNext(2)
  assert(sig1() == 2)

  val sub2 = Subject[Int]()
  val sig2 = sub2.toSignal
  sub2.onNext(1)
  val increment = sig2.map(_ + 1)
  assert(increment() == 2)
  sub2.onNext(2)
  assert(increment() == 3)

  val sub31 = Subject[Int]()
  val sub32 = Subject[String]()
  val sig31 = sub31.toSignal
  val sig32 = sub32.toSignal
  sub31.onNext(1)
  sub32.onNext("a")
  val zipped = sig31.zip(sig32)
  assert(zipped() == (1, "a"))
  sub31.onNext(2)
  sub32.onNext("b")
  assert(zipped() == (2, "b"))

  val sub4 = Subject[Int]()
  val sig4 = sub4.toSignal
  sub4.onNext(1)
  val sum = sig4.scan(10)(_ + _)
  assert(sum() == 10)
  sub4.onNext(2)
  assert(sum() == 12)
  sub4.onNext(3)
  assert(sum() == 15)

}

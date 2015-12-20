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

object Ex4 extends App {

  import rx.lang.scala._

  implicit class ObserverableAdditional[T](val self:Observable[T]) extends AnyVal {

    def toSignal:Signal[T] = {
      val s = new Signal[T]
      self.last.subscribe(s.subject)
      s
    }

  }

  class Signal[T] {

    def this(t:T) {
      this()
      a = t
    }

    var a:T = _

    val subject = Subject[T]()

    subject.subscribe(a = _)

    def apply(): T = a

    def map[S](f: T => S): Signal[S] = new Signal[S](f(a))

    def zip[S](that: Signal[S]): Signal[(T, S)] = new Signal[(T,S)]((a,that.a))

    def scan[S](z: S)(f: (S, T) => S):Signal[S] = new Signal[S](f(z,a))
  }

  //test
  val s1 = Observable.items[String]("A","B","C").toSignal
  log(s"element = ${s1()}")

  val s2 = Observable.items[Int](1,2,3).toSignal

  val sMap = s1.map(_+"~")
  log(s"sMap: element = ${sMap()}")

  val sZip = s1.zip(s2)
  log(s"sZip: element = ${sZip()}")

  val sScan = s2.scan(10)((s,t)=>s+t)
  log(s"sScan: element = ${sScan()}")



}

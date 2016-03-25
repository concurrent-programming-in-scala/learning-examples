package org.learningconcurrency
package exercises
package ch6

/**
 * Implement the reactive cell abstraction, represented with the RCell[T] type:
 * class RCell[T] extends Signal[T] {
 *   def :=(x: T): Unit = ???
 * }
 *
 * A reactive cell is simultaneously a reactive signal from the previous exercise.
 * Calling the := method sets a new value to the reactive cell, and emits an event.
 */

import rx.lang.scala._

object Ex5 extends App {

  class RCell[T] extends Ex4.Signal[T] {
    private[this] val subject = Subject[T]()
    setObservable(subject)

    def :=(x: T): Unit = {
      subject.onNext(x)
    }
  }

  val rc1 = new RCell[Int]()
  rc1 := 1
  assert(rc1() == 1)

  val rc2 = new RCell[Int]()
  rc2 := 1
  val increment = rc2.map(_ + 1)
  assert(increment() == 2)
  rc2 := 2
  assert(increment() == 3)

  val rc31 = new RCell[Int]()
  val rc32 = new RCell[String]()
  rc31 := 1
  rc32 := "a"
  val zipped = rc31.zip(rc32)
  assert(zipped() == (1, "a"))
  rc31 := 2
  rc32 := "b"
  assert(zipped() == (2, "b"))

  val rc4 = new RCell[Int]()
  rc4 := 1
  val sum = rc4.scan(10)(_ + _)
  assert(sum() == 10)
  rc4 := 2
  assert(sum() == 12)
  rc4 := 3
  assert(sum() == 15)

}
package org.learningconcurrency
package exercises
package ch6

/**
 * Implement the reactive priority queue, represented with the RPriorityQueue class:
 * class RPriorityQueue[T] {
 *   def add(x: T): Unit = ???
 *   def pop(): T = ???
 *   def popped: Observable[T] = ???
 * }
 *
 * The reactive priority queue exposes the Observable object popped,
 * which emits events whenever the smallest element in the priority queue gets removed by calling pop.
 */

import rx.lang.scala._

object Ex7 extends App {

  class RPriorityQueue[T](implicit val ord: Ordering[T]) {
    private[this] val pq = new scala.collection.mutable.PriorityQueue[T]()(ord.reverse)
    private[this] val subject = Subject[T]()

    def add(x: T): Unit = {
      pq += x
    }

    /* This method throws `NoSuchElementException` if the queue is empty. */
    def pop(): T = {
      val x = pq.dequeue()
      subject.onNext(x)
      x
    }

    def popped: Observable[T] = subject
  }

  import scala.collection.mutable.ListBuffer

  val rqueue = new RPriorityQueue[Int]()
  rqueue.add(3)
  rqueue.add(1)
  rqueue.add(2)

  val o = rqueue.popped
  val buf = ListBuffer.empty[Int]
  o.subscribe(buf += _)

  assert(rqueue.pop() == 1)
  assert(rqueue.pop() == 2)
  assert(rqueue.pop() == 3)
  assert(buf == ListBuffer(1, 2, 3))

}
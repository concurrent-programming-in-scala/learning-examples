package org.learningconcurrency
package exercises
package ch3

import java.util.concurrent.atomic.AtomicReference

import org.learningconcurrency.ch2._

import scala.annotation.tailrec

/**
 * Implement a ConcurrentSortedList class, which implements a concurrent
 *
 * class ConcurrentSortedList[T](implicit val ord: Ordering[T]) {
 * def add(x: T): Unit = ???
 * def iterator: Iterator[T] = ???
 * }
 *
 * Under the hood, the ConcurrentSortedList class should use a linked list of atomic references.
 * Ensure that your implementation is lock-free and avoids ABA problems.
 * The Iterator object returned by the iterator method must correctly traverse the elements of the list
 * in the ascending order under the assumption that there are no concurrent invocations of the add method.
 *
 * If required, modify the ConcurrentSortedList class from the previous example so 
 * that calling the add method has the running time linear to the length of the list 
 * and creates a constant number of new objects when there are no retries due to concurrent add invocations.
 */


object Ex3_4 extends App {

  class ConcurrentSortedList[T](implicit val ord: Ordering[T]) {

    private val h: AtomicReference[Option[T]] = new AtomicReference[Option[T]](None)
    private val t: AtomicReference[Option[ConcurrentSortedList[T]]] = new AtomicReference[Option[ConcurrentSortedList[T]]](None)

    @tailrec
    private def addToTail(x: T): Unit = {
      val v = t.get
      v match {
        case Some(p) => p.add(x)
        case None => {
          val l = new ConcurrentSortedList[T]
          l.h.set(Some(x))
          if (!t.compareAndSet(v, Some(l))) addToTail(x)
        }
      }
    }

    @tailrec
    final def add(x: T): Unit = {
      val optV = h.get

      optV match {
        case Some(v) => {
          if (ord.compare(v, x) >= 0) {
            if (h.compareAndSet(optV, Some(x))) addToTail(v)
            else add(x)
          } else addToTail(x)
        }
        case None => {
          if (!h.compareAndSet(optV, Some(x)))
            add(x)
        }
      }
    }

    def iterator: Iterator[T] = new Iterator[T] {

      var rIter: Option[ConcurrentSortedList[T]] = Some(ConcurrentSortedList.this)

      override def hasNext: Boolean = rIter.isEmpty == false

      override def next(): T = {

        rIter match {
          case Some(r) => {
            rIter = r.t.get
            r.h.get match {
              case Some(v) => v
              case None => throw new NoSuchElementException("next on empty iterator")
            }
          }
          case None => throw new NoSuchElementException("next on empty iterator")
        }
      }
    }

  }

  val csl = new ConcurrentSortedList[Int]()


  (1 to 10).map((i) => thread {
    Thread.sleep((Math.random() * 100).toInt)
    for (i <- 1 to 10)
      csl.add((math.random * 100 + i).toInt)
  }
  ).foreach(_.join)

  log(s"length = ${csl.iterator.length}")

  for (a <- csl.iterator) {
    log(a.toString)
  }

}

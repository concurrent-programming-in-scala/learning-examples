package org.learningconcurrency
package exercises
package ch3

import java.util.ConcurrentModificationException
import java.util.concurrent.atomic.AtomicReference

import org.learningconcurrency.ch2._

/**
 * Implement a ConcurrentSortedList class, which implements a concurrent
 *
 * class ConcurrentSortedList[T](implicit val ord: Ordering[T]) {
 *   def add(x: T): Unit = ???
 *     def iterator: Iterator[T] = ???
 *   }
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

    val r = new AtomicReference[List[T]](List.empty[T])

    def addWithSort(x:T,l:List[T]):List[T] = l match {
      case h::_ if ord.compare(h,x) >= 0 => x::l
      case h::t => h::addWithSort(x,t)
      case _ => List(x)
    }

    def add(x: T): Unit = {
      val oldList = r.get
      val newList = addWithSort(x,oldList)

      if (!r.compareAndSet(oldList,newList)) add(x)
    }

    def iterator: Iterator[T] = new Iterator[T] {

      val rIter = new AtomicReference(r.get)

      override def hasNext: Boolean = {
        !rIter.get.isEmpty
      }

      override def next(): T = {
        val l = rIter.get

        if (l.isEmpty) throw new NoSuchElementException("next on empty iterator")

        if (!rIter.compareAndSet(l,l.tail))
          throw new ConcurrentModificationException()

        l.head
      }

    }

  }

  val csl = new ConcurrentSortedList[Int]()


  (1 to 10).map((i) => thread {
    for (i <- 1 to 10)
      csl.add((math.random * 100 + i).toInt)
      Thread.sleep(1)
    }
  ).foreach(_.join)

  for (a <- csl.iterator) {
    log(a.toString)
  }

}

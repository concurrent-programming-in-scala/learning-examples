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

    case class Node(head: T,
                    tail: AtomicReference[Option[Node]] = new AtomicReference[Option[Node]](None))

    val root = new AtomicReference[Option[Node]](None)

    @tailrec
    private def add(r: AtomicReference[Option[Node]], x: T): Unit = {
      val optNode = r.get

      optNode match {
        case None => {
          if (!r.compareAndSet(optNode, Some(Node(x)))) add(r, x)
        }
        case Some(Node(head, tail)) =>
          if (ord.compare(x, head) <= 0) {
            //prepend new node
            val newNode = Node(x)
            newNode.tail.set(optNode)
            if (!r.compareAndSet(optNode, Some(newNode))) add(r, x)
          } else {
            //add to tail
            add(tail, x)
          }
      }
    }

    def add(x: T): Unit = {
      add(root, x)
    }

    def iterator: Iterator[T] = new Iterator[T] {

      var rIter = root.get

      override def hasNext: Boolean = rIter.isEmpty == false

      override def next(): T = {

        rIter match {
          case Some(node) => {
            rIter = node.tail.get
            node.head
          }
          case None => throw new NoSuchElementException("next on empty iterator")
        }
      }
    }
  }

  val csl = new ConcurrentSortedList[Int]()


  (1 to 100).map((i) => thread {
    Thread.sleep((Math.random() * 100).toInt)
    for (i <- 1 to 1000) {
      Thread.sleep((Math.random() * 10).toInt)
      csl.add((math.random * 100 + i).toInt)
    }
  }
  ).foreach(_.join)

  log(s"length = ${csl.iterator.length}")

  var prev = 0
  var length = 0
  for (a <- csl.iterator) {
    log(a.toString)
    if (prev > a) throw new Exception(s"$prev > $a")
    prev = a
    length += 1
  }

  if (csl.iterator.length != length) throw new Exception(s"${csl.iterator.length} != $length")

  log(s"length = ${csl.iterator.length} ($length)")

}
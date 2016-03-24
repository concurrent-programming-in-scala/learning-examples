package org.learningconcurrency
package exercises
package ch6

/**
 * Implement the reactive map collection, represented with the RMap class:
 * class RMap[K, V] {
 *   def update(k: K, v: V): Unit
 *   def apply(k: K): Observable[V]
 * }
 *
 * The update method behaves like the update on a regular Map collection.
 * Calling apply on a reactive map returns an Observable object with all the subsequent updates of the specific key.
 */

import rx.lang.scala._

object Ex6 extends App {

  class RMap[K, V] {
    import scala.collection._
    private[this] val allSubscribers = mutable.Map[K, (Subject[V], mutable.Set[Subscriber[V]])]()
    private[this] val map = mutable.Map[K, V]()

    def update(k: K, v: V): Unit = {
      map(k) = v
      allSubscribers.get(k) match {
        case Some(s) => s._1.onNext(v)
        case _ =>
      }
    }

    def apply(k: K): Observable[V] = Observable[V] { subscriber =>
      val (subject, subscribers) =
        allSubscribers.getOrElseUpdate(k, (Subject[V](), mutable.Set.empty[Subscriber[V]]))
      subscribers += subscriber

      val subscription = subject.subscribe(subscriber)

      subscriber.add(Subscription {
        subscription.unsubscribe()

        subscribers -= subscriber
        if (subscribers.isEmpty) {
          allSubscribers -= k
        }
      })
    }

    /* return true if there is at least one subscriber which subscribes to the updates of the specific key. */
    def hasSubscribers(k: K): Boolean = allSubscribers.get(k).isDefined
  }

  import scala.collection.mutable.ListBuffer

  val rmap = new RMap[String, Int]()

  val key = "a"
  val o = rmap(key)
  assert(rmap.hasSubscribers(key) == false)

  val buf1 = ListBuffer.empty[Int]
  val subscription1 = o.subscribe(buf1 += _)
  val buf2 = ListBuffer.empty[Int]
  val subscription2 = o.subscribe(buf2 += _)

  rmap(key) = 1
  rmap(key) = 2
  assert(buf1 == ListBuffer(1, 2), buf1)
  assert(buf2 == ListBuffer(1, 2), buf2)

  subscription1.unsubscribe()
  assert(rmap.hasSubscribers(key))
  subscription2.unsubscribe()
  assert(rmap.hasSubscribers(key) == false)

}
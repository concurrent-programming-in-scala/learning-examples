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
    private[this] val map = scala.collection.mutable.Map[K, Subject[V]]()

    def update(k: K, v: V): Unit = map.get(k) match {
      case Some(s) => s.onNext(v)
      case _ =>
        val s = Subject[V]()
        map(k) = s
        s.onNext(v)
    }

    /* This method throws `NoSuchElementException` if the key does not exist in the map. */
    def apply(k: K): Observable[V] = map.get(k).get
  }

  import scala.collection.mutable.ListBuffer

  val rmap = new RMap[String, Int]()
  rmap("a") = 1

  val o = rmap("a")
  val buf = ListBuffer.empty[Int]
  o.subscribe(buf += _)

  rmap("a") = 2
  rmap("a") = 3

  assert(buf == ListBuffer(2, 3), buf)

}
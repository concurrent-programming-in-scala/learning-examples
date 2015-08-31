package org.learningconcurrency
package exercises
package ch3

import scala.collection._

/**
 * Implement a SyncConcurrentMap class that extends the Map interface from the scala.collection.concurrent package.
 * Use the synchronized statement to protect the state of the concurrent map.
 */

object Ex7 extends App {

  class SyncConcurrentMap[A, B] extends scala.collection.concurrent.Map[A, B] {

    private val m = mutable.Map.empty[A, B]

    override def putIfAbsent(k: A, v: B): Option[B] = m synchronized {
      m.get(k) match {
        case optV@Some(_) => optV
        case None => m.put(k, v)
      }
    }

    def replace(k: A, oldvalue: B, newvalue: B): Boolean = m synchronized {
      m.get(k) match {
        case Some(v) if ((v != null) && v.equals(oldvalue)) || ((v == null) && (oldvalue == null)) => m.put(k, newvalue); true
        case _ => false
      }
    }

    def remove(k: A, v: B): Boolean = m synchronized {
      m.get(k) match {
        case Some(oldvalue) if ((oldvalue != null) && oldvalue.equals(v)) || ((v == null) && (oldvalue == null)) => m.remove(k); true
        case _ => false
      }
    }

    override def replace(k: A, v: B): Option[B] = m synchronized {
      m.get(k) match {
        case old@Some(oldvalue) => m.put(k, v); old
        case None => None
      }

    }

    override def +=(kv: (A, B)): SyncConcurrentMap.this.type = m synchronized {
      m.put(kv._1, kv._2)
      this
    }

    override def -=(key: A): SyncConcurrentMap.this.type = m synchronized {
      m.remove(key)
      this
    }

    override def get(key: A): Option[B] = m synchronized {
      m.get(key)
    }

    override def iterator: scala.Iterator[(A, B)] = m synchronized {
      m.iterator
    }
  }

  val m = new SyncConcurrentMap[Int, String]()


  import org.learningconcurrency.ch2.thread

  val t = (1 to 100).map((i) => thread {
    (1 to 100).foreach {
      (k) => {
        val v = s"${Thread.currentThread().getName}"
        m.put(k, v)
        log(s"-> ($k,$v)")
      }
    }
  })

  Thread.sleep(100)

  for ((k, v) <- m) {
    log(s"<- ($k,$v)")
  }

  t.foreach(_.join)

}

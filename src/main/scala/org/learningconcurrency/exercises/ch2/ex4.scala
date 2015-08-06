package org.learningconcurrency
package exercises
package ch2

object Ex4 extends App {

  class SyncVar[T] {

    private var empty: Boolean = true

    private var x: T = null.asInstanceOf[T]

    def get(): T = this.synchronized {
      if (empty) throw new Exception("must be non-empty")
      else {
        empty = true
        x
      }
    }

    def put(x: T): Unit = this.synchronized {
      if (!empty) throw new Exception("must be empty")
      else {
        empty = false
        this.x = x
      }
    }

    def isEmpty = empty

    def nonEmpty = !empty

  }

  import org.learningconcurrency.ch2.thread

  val lock = new AnyRef

  val syncVar = new SyncVar[Int]

  val producer = thread {
    var x = 0
    while(x < 15) {

      lock.synchronized {
        if (syncVar.isEmpty) {
          syncVar.put(x)
          x = x + 1
        }
        lock.wait
      }

    }
  }

  val consumer = thread {
    var x = -1
    while(x < 14) {
      lock.synchronized {
        if (syncVar.nonEmpty) {
          x = syncVar.get
          log(s"get = $x")
        }
        lock.notify
      }
    }
  }

  producer.join
  consumer.join

}

package org.learningconcurrency
package exercises
package ch2

object Ex5 extends App {

  class SyncVar[T] {

    private var empty: Boolean = true

    private var x: T = null.asInstanceOf[T]

    def isEmpty = synchronized {
      empty
    }

    def nonEmpty = synchronized {
      !empty
    }

    def getWait():T = this.synchronized {
      while (empty)
        this.wait()

      empty = true
      this.notify()
      x
    }

    def putWait(x: T): Unit = this.synchronized {
      while (!empty)
        this.wait()

      empty = false
      this.x = x
      this.notify()
    }


  }

  import org.learningconcurrency.ch2.thread

  val syncVar = new SyncVar[Int]

  val producer = thread {
    var x = 0
    while(x < 15) {
      syncVar.putWait(x)
      x = x + 1
    }
  }

  val consumer = thread {
    var x = -1
    while(x < 14) {
      x = syncVar.getWait
      log(s"get: $x")
    }
  }

  producer.join()
  consumer.join()

}

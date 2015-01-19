package org.learningconcurrency
package exercises
package ch2

object Ex2 extends App {

  def periodically(duration: Long)(f: () => Unit): Unit = {
    val worker = new Thread {
      while (true) {
        f()
        Thread.sleep(duration)
      }
    }

    worker.setName("Worker")
    worker.setDaemon(true)
    worker.start()
  }

}

package org.learningconcurrency
package exercises
package ch2

import org.learningconcurrency.ch2.thread

object Ex1 extends App {

  def parallel[A, B](a: =>A, b: =>B): (A, B) = {
    val t1 = thread {
      a
    }
    val t2 = thread {
      b
    }
    t1.join()
    t2.join()
    (a, b)
  }

}

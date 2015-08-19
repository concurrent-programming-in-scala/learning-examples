package org.learningconcurrency
package exercises
package ch2

object Ex3 extends App {

  class SyncVar[T] {

    private var empty:Boolean = true

    private var x:T = null.asInstanceOf[T]

    def get(): T = this.synchronized {
      if (empty) throw new Exception("must be non-empty")
      else {
        empty = true
        val v = x
        x = null.asInstanceOf[T]
        v
      }
    }

    def put(x: T):Unit = this.synchronized {
      if (!empty) throw new Exception("must be empty")
      else {
        empty = false
        this.x = x
      }
    }

  }

}

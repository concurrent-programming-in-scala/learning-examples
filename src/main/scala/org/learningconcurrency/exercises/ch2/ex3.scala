package org.learningconcurrency
package exercises
package ch2

object Ex3 extends App {

  class SyncVar[T] {
    private var state: Option[T] = None

    def get(): T = this.synchronized {
      state.fold(throw new Exception("No value"))(identity)
    }
    def put(x: T): Unit = this.synchronized {
      state.fold{state = Some(x); ()}(throw new Exception("Already has value"))
    }
  }

}

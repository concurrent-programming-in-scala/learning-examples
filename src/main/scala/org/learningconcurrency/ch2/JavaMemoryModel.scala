package org.learningconcurrency
package ch2






object JMMPublicationWrong extends App {
  class Num(var x: Int)
  var num = new Num(-1)
  val t = thread {
    for (i <- 1 until 10000000) num = new Num(i)
  }
  while (t.isAlive) assert(num.x != 0)
}


object JMMPublicationRight extends App {
  class Num(val x: Int)
  @volatile var num = new Num(-1)
  val t = thread {
    for (i <- 1 until 10000000) num = new Num(i)
  }
  while (t.isAlive) assert(num.x != 0)
}

//  rules for happens-before are:
// Program order rule. Each action in a thread happens-before every ac-
// tion in that thread that comes later in the program order.
// Monitor lock rule. An unlock on a monitor lock happens-before every
// subsequent lock on that same monitor lock.3
// Volatile variable rule. A write to a volatile field happens-before every
// subsequent read of that same field.4
// Thread start rule. A call to Thread.start on a thread happens-before
// every action in the started thread.
// Thread termination rule. Any action in a thread happens-before any other thread detects that thread has terminated, either by success- fully return from Thread.join or by Thread.isAlive returning false.
// Interruption rule. A thread calling interrupt on another thread happens-before the interrupted thread detects the interrupt (either by having InterruptedException thrown, or invoking isInter- rupted or interrupted).
// Finalizer rule. The end of a constructor for an object happens-before the start of the finalizer for that object.
// Transitivity. If A happens-before B, and B happens-before C, then A happens-before C.



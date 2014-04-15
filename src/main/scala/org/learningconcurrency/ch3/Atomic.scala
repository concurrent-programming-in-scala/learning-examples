package org.learningconcurrency
package ch3






object AtomicUid extends App {
  import java.util.concurrent.atomic._
  import scala.concurrent._
  import scala.annotation.tailrec
  private val uid = new AtomicLong(0L)

  @tailrec def getUniqueId(): Long = {
    val oldUid = uid.get()
    val newUid = oldUid + 1
    if (uid.compareAndSet(oldUid, newUid)) newUid
    else getUniqueId()
  }

  execute(runnable {
    log(s"Got a unique id asynchronously: $getUniqueId")
  })

  log(s"Got a unique id: $getUniqueId")
}


object AtomicStack {
  import java.util.concurrent.atomic._
  import scala.concurrent._
  import scala.annotation.tailrec

  trait Stack
  case class Node(head: Int, tail: Stack) extends Stack
  case object Bottom extends Stack

  private val stack = new AtomicReference[Stack](Bottom)
  @tailrec def push(x: Int) {
    val oldTop = stack.get()
    val newTop = Node(x, oldTop)
    if (!stack.compareAndSet(oldTop, newTop)) push(x)
  }
  @tailrec def pop(): Option[Int] = {
    stack.get() match {
      case Bottom => None
      case oldTop @ Node(head, newTop) =>
        if (stack.compareAndSet(oldTop, newTop)) Some(head)
        else pop()
    }
  }

  def main(args: Array[String]) {
    execute(runnable {
      @tailrec def poll() {
        pop() match {
          case Some(-1) =>
            log("Got -1. Done!")
          case Some(x) =>
            log(s"Got $x")
            poll()
          case None =>
            poll()
        }
      }
      poll()
    })
  
    push(1)
    push(2)
    push(3)
    Thread.sleep(100)
    push(-1)
  }

}


object AtomicArrays extends App {
  import java.util.concurrent.atomic._
  import scala.concurrent._

  private val counts = new AtomicIntegerArray(4)
  def lowerBound(): Int = {
    var cnt = 0
    for (i <- 0 until counts.length) cnt += counts.get(i)
    cnt
  }

  for (i <- 0 until counts.length) execute(runnable {
    for (_ <- 0 until 200) counts.incrementAndGet(i)
  })

  log(s"Count lower bound: ${lowerBound()}")
  log(s"Count lower bound: ${lowerBound()}")
  log(s"Count lower bound: ${lowerBound()}")
}





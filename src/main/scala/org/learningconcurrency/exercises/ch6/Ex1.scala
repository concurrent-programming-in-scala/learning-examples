package org.learningconcurrency
package exercises
package ch6

/**
  * Implement a custom Observable[Thread] object that emits an event when it detects that a thread was started.
  * The implementation is allowed to miss some of the events.
  */

import java.util.Calendar

import rx.lang.scala.Observable

import scala.annotation.tailrec
import scala.concurrent.duration._

object Ex1 extends App {

  val rootThreadGroup = getRootThread(Thread.currentThread.getThreadGroup)

  var existsThreads = Set.empty[Thread]

  @tailrec
  def getRootThread(t: ThreadGroup):ThreadGroup = {
    val parent = t.getParent
    if (parent == null) t else getRootThread(parent)
  }

  def getCurrentThreads = {
    val threads = new Array[Thread](rootThreadGroup.activeCount())
    rootThreadGroup.enumerate(threads,true)

    threads.filter(_ != null)
  }

  def getNewThreads = {
    val currentThreads = getCurrentThreads
    val newThreads = currentThreads.filter(!existsThreads.contains(_))

    //save threads
    existsThreads = currentThreads.toSet

    newThreads
  }

  def createObservableNewThreads: Observable[Thread] = {
    Observable[Thread] {
      (s) => {
        getNewThreads.foreach(s.onNext _)
      }
    }
  }

  //create Observable
  val o = for {
    _ <- Observable.interval(1 seconds)
    j <- createObservableNewThreads
  } yield j

  o.subscribe((t) => log(s"${Calendar.getInstance().getTime()}: ${t.toString}"))

  //test

  def createTestThread(name:String): Unit = {
    val t = new Thread(name) {
      override def run(): Unit = {
        Thread.sleep(5000)
      }
    }
    t.start()
  }

  Thread.sleep(2000)
  createTestThread("A")
  Thread.sleep(3000)
  createTestThread("B")

  Thread.sleep(10000)
}

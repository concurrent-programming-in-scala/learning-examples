package org.learningconcurrency
package exercises
package ch2

import org.learningconcurrency._

import scala.collection.mutable

object Ex9 extends App {

  class PriorityTaskPool(val p:Int) {

    implicit val ord: Ordering[(Int,() => Unit)] = Ordering.by(_._1)

    private val tasks = mutable.PriorityQueue[(Int,() => Unit)]()

    def asynchronous(priority: Int)(task: => Unit):Unit = tasks synchronized {
      tasks.enqueue((priority,() => task))
      tasks.notify()
    }

    class Worker extends Thread {

      setDaemon(true)

      def poll() = tasks.synchronized {
        while (tasks.isEmpty) {
          tasks.wait()
        }
        log("queue: " + tasks.foldLeft("")((s,t)=>s"$s${t._1},"))
        tasks.dequeue()
      }

      override def run() = {
        while (true) {
          poll() match {
            case (_, task) => task()
          }
        }
      }
    }

    (1 to p).map((i) => new Worker()).map(_.start)

  }

  val tasks = new PriorityTaskPool(10)

  (1 to 100).foreach((i) => {
    val a = (Math.random*1000).toInt
    tasks.asynchronous(a)({log(s"<- $a")})
  })

  Thread.sleep(10000)

}

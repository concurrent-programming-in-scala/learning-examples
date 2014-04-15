package org.learningconcurrency
package ch3






object ExecutorsCreate extends App {
  import scala.concurrent._
  val executor = new forkjoin.ForkJoinPool
  try {
    val f = executor.submit(new Runnable {
      def run() = log("This task is run asynchronously.")
    })
    f.get()
    log("The asynchronous task completed.")
  } finally {
    executor.shutdown()
  }
}


object ExecutionContextGlobal extends App {
  import scala.concurrent._
  val ectx = ExecutionContext.global
  ectx.execute(runnable {
    log("Running on the execution context.")
  })
}


object ExecutionContextCreate extends App {
  import java.util.concurrent._
  import scala.concurrent._
  val ectx = ExecutionContext.fromExecutorService(new forkjoin.ForkJoinPool)
  try {
    ectx.execute(runnable {
      log("Running on the execution context again.")
    })
  } finally {
    ectx.shutdown()
  }
}



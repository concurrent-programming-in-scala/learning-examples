package org.learningconcurrency
package exercises
package ch4

import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

/**
 * Implement a command-line program that asks the user to input a URL of some website,
 * and displays the HTML of that website.
 *
 * Between the time that the user hits ENTER and the time that the HTML is retrieved,
 * the program should repetitively print a . to the standard output every 50 milliseconds,
 * with a two seconds timeout.
 *
 * Use only futures and promises, and avoid the synchronization primitives from the previous chapters.
 * You may reuse the timeout method defined in this chapter.
 */

object Ex1 extends App {

  import java.util._

  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.io.Source

  private val timer = new Timer(true)

  def stopTimer(t:Timer) = {
    t.cancel()
    t.purge()
  }

  def timeout(p: Promise[String], t: Long): Unit = {
    timer.schedule(
      new TimerTask {
        def run() = {
          p trySuccess (s"Sorry, timed out ($t ms)")
        }
      }, t
    )
  }

  def timeOutPrinter(t:Timer): Unit = {
    t.schedule(
      new TimerTask {
        override def run(): Unit = print(".")
      },0,50
    )
  }

  while (true) {
    println("---------------------------------------------")
    println("Please, input URL")
    val url = scala.io.StdIn.readLine

    val dotPrinterTimer = new Timer(true)

    val p = Promise[String]

    val reader = Future {
      timeout(p, 2000)
      Source.fromURL(url).mkString
    } onComplete {
      case Success(s) => p.trySuccess(s)
      case Failure(e) => p.trySuccess(s"Error !!!! ${e.toString}")
    }

    val printer = Future {
      println(s"Reading from $url, please wait ")
      timeOutPrinter(dotPrinterTimer)
    }

    val l = Await.result(p.future, Duration.Inf)

    stopTimer(dotPrinterTimer)
    println("")
    println(l)

  }

}

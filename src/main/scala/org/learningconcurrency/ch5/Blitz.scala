package org.learningconcurrency
package ch5



import scala.collection._
import scala.collection.par._
import scala.collection.par.Scheduler.Implicits.global



object BlitzComparison extends App {
  val array = (0 until 100000).toArray
  @volatile var x = 0

  val seqtime = warmedTimed(1000) {
    array.reduce(_ + _)
  }
  val partime = warmedTimed(1000) {
    array.par.reduce(_ + _)
  }
  val blitztime = warmedTimed(1000) {
    x = array.toPar.reduce(_ + _)
  }

  log(s"sequential time - $seqtime")
  log(s"parallel time   - $partime")
  log(s"ScalaBlitz time - $blitztime")
}


object BlitzHierarchy extends App {
  val array = (0 until 100000).toArray
  val range = 0 until 100000

  def sum(xs: Zippable[Int]): Int = {
    xs.reduce(_ + _)
  }

  println(sum(array.toPar))

  println(sum(range.toPar))

}




package org.learningconcurrency
package exercises
package ch6

import rx.lang.scala._
import rx.lang.scala.Observable._

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

/**
  * Use the randomQuote method from this section in order to create an Observable object
  * with the moving average of the quote lengths.
  * Each time a new quote arrives, a new average value should be emitted.
  */
object Ex3 extends App {

  import scala.concurrent.duration._

  @tailrec
  def randomString(length: Int, l: List[Char] = List.empty[Char]):List[Char] = {
    if (length == 1) util.Random.nextPrintableChar :: l
    else randomString(length-1,util.Random.nextPrintableChar :: l)

  }

  def randomQuoteMock = Observable.interval(1 seconds).map((l) => randomString(Random.nextInt(10)+1))

  randomQuoteMock.scan((0D,0)){
    (n,q) => n match {
      case (s,c) => (s + q.length, c + 1)
    }
  }.filter(_._2 != 0).map((e) => e._1/e._2).subscribe((e) => log(s"avg = $e"))

  Thread.sleep(10000)

}

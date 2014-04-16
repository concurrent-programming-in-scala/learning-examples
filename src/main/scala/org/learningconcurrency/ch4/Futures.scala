package org.learningconcurrency
package ch4






object FuturesCreate extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global

  Future {
    log(s"the future is here")
  }

  log(s"future started")

}


object FuturesType extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global

  val buildFile: Future[String] = Future {
    val f = scala.io.Source.fromFile("build.sbt")
    try f.getLines.mkString("\n") finally f.close()
  }

  log(s"started reading build file asynchronously")
  log(s"status: ${buildFile.isCompleted}")
  Thread.sleep(250)
  log(s"status: ${buildFile.isCompleted}")

}


object FuturesCallbacks extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global

  val urlSpec: Future[String] = Future {
    val f = scala.io.Source.fromURL("http://www.w3.org/Addressing/URL/url-spec.txt")
    try f.getLines.mkString("\n") finally f.close()
  }

  urlSpec onSuccess {
    case text => log(text)
  }

}



package org.learningconcurrency



import scala.concurrent._



package object ch3 {

  def runnable(body: =>Unit) = new Runnable {
    def run() = body
  }

  def execute(r: Runnable) = ExecutionContext.global.execute(r)

}


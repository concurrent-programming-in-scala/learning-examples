package org.learningconcurrency



import scala.concurrent._



package object ch3 {

  def execute(body: =>Unit) = ExecutionContext.global.execute(new Runnable {
    def run() = body
  })

}


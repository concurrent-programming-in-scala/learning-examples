package org.learningconcurrency






package object ch2 {

  def thread(body: =>Unit): Thread = {
    val t = new Thread {
      override def run() = body
    }
    t.start()
    t
  }

}


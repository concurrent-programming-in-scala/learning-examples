package org.learningconcurrency






package object ch5 {
  def timed[T](body: =>T): Double = {
    val start = System.nanoTime
    body
    val end = System.nanoTime
    ((end - start) / 1000) / 1000.0
  }

  def warmedTimed[T](times: Int = 200)(body: =>T): Double = {
    for (_ <- 0 until times) body
    timed(body)
  }
}


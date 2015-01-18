package org.learningconcurrency.exercises

object CH2Solutions extends App{

  def parallel[A, B](a: =>A, b: =>B): (A, B) = {
    var aVal: A = null.asInstanceOf[A]
    var bVal: B = null.asInstanceOf[B] 
    
    val t1 = thread {
      aVal = a
      log(aVal.toString())
    }

    val t2 = thread {
      bVal = b
      log(bVal.toString())
    }

    t1.join()
    t2.join()

    (aVal, bVal)
  }

  def periodically(duration: Long)(f: () => Unit): Unit = {
    for (i <- 1 to 3){
      thread {
        f()
        Thread.sleep(duration)
      }.join()
    }
  }

}

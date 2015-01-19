package org.learningconcurrency.exercises

import org.learningconcurrency.ch2.thread


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
      val worker = new Thread {
        while(true){
          f()
          Thread.sleep(duration)
        }
      }

      worker.setName("Worker")
      worker.setDaemon(true)
      worker.start()
    }
  }

}

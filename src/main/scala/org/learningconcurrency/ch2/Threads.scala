package org.learningconcurrency
package ch2






object ThreadsCreation extends App {

  val t = new Thread {
    override def run(): Unit = {
      println("New thread running.")
    }
  }

  t.start()
  t.join()
  println("New thread joined.")

}


object ThreadsSleep extends App {

  val t = thread {
    Thread.sleep(1000)
    println("New thread running.")
    Thread.sleep(1000)
    println("Still running.")
    Thread.sleep(1000)
    println("Completed.")
  }
  t.join()
  println("New thread joined.")

}


object ThreadsNondeterminism extends App {

  val t = thread {
    println("New thread running.")
  }
  println("...")
  println("...")
  println("...")
  println("...")
  println("...")
  t.join()
  println("New thread joined.")

}


object ThreadsUnprotectedUid extends App {

  var uidCount = 0L

  def getUniqueId() = {
    val freshUid = uidCount + 1
    uidCount = freshUid
    freshUid
  }

  def getUniqueIds(n: Int): Unit = {
    val uids = for (i <- 0 until n) yield getUniqueId()
    println(s"Generated uids: $uids")
  }

  val t = thread {
    getUniqueIds(5)
  }
  getUniqueIds(5)
  t.join()

}


object ThreadSharedStateAccessReordering extends App {
  for (i <- 0 until 10000) {
    var t1started = false
    var t2started = false
    var t1index = 0
    var t2index = 0

    val t1 = thread {
      Thread.sleep(1)
      t1started = true
      t2index = if (t2started) 0 else 1
    }
    val t2 = thread {
      Thread.sleep(1)
      t2started = true
      t1index = if (t1started) 0 else 1
    }
  
    t1.join()
    t2.join()
    assert(!(t1index == 1 && t2index == 1), s"t1 = $t1index, t2 = $t2index")
  }
}

















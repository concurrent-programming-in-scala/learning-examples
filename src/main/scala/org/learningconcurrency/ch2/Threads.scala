package org.learningconcurrency
package ch2






object ThreadsCreation extends App {

  class MyThread extends Thread {
    override def run(): Unit = {
      println("New thread running.")
    }
  }
  val t = new MyThread

  t.start()
  t.join()
  println("New thread joined.")

}


object ThreadsSleep extends App {

  val t = thread {
    Thread.sleep(1000)
    log("New thread running.")
    Thread.sleep(1000)
    log("Still running.")
    Thread.sleep(1000)
    log("Completed.")
  }
  t.join()
  log("New thread joined.")

}


object ThreadsNondeterminism extends App {

  val t = thread { log("New thread running.") }
  log("...")
  log("...")
  t.join()
  log("New thread joined.")

}


object ThreadsCommunicate extends App {
  var result: String = null
  val t = thread { result = "\nTitle\n" + "=" * 5 }
  t.join()
  log(result)
}


object ThreadsUnprotectedUid extends App {

  var uidCount = 0L

  def getUniqueId() = {
    val freshUid = uidCount + 1
    uidCount = freshUid
    freshUid
  }

  def printUniqueIds(n: Int): Unit = {
    val uids = for (i <- 0 until n) yield getUniqueId()
    log(s"Generated uids: $uids")
  }

  val t = thread {
    printUniqueIds(5)
  }
  printUniqueIds(5)
  t.join()

}


object ThreadSharedStateAccessReordering extends App {
  for (i <- 0 until 10000) {
    var t1started = false
    var t2started = false
    var t1index = -1
    var t2index = -1

    val t1 = thread {
      Thread.sleep(2)
      t1started = true
      t2index = if (t2started) 0 else 1
    }
    val t2 = thread {
      Thread.sleep(2)
      t2started = true
      t1index = if (t1started) 0 else 1
    }
  
    t1.join()
    t2.join()
    assert(!(t1index == 1 && t2index == 1), s"t1 = $t1index, t2 = $t2index")
  }
}

















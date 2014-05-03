package org.learningconcurrency
package ch2






object SynchronizedProtectedUid extends App {

  var uidCount = 0L

  def getUniqueId() = this.synchronized {
    val freshUid = uidCount + 1
    uidCount = freshUid
    freshUid
  }

  def printUniqueIds(n: Int): Unit = {
    val uids = for (i <- 0 until n) yield getUniqueId()
    println(s"Generated uids: $uids")
  }

  val t = thread{
    printUniqueIds(5)
  }
  printUniqueIds(5)
  t.join()

}


// we should skip this one
object SynchronizedSharedStateAccess extends App {
  for (i <- 0 until 10000) {
    var t1started = false
    var t2started = false
    var t1index = 0
    var t2index = 0
  
    val t1 = thread {
      Thread.sleep(1)
      this.synchronized { t1started = true }
      val t2s = this.synchronized { t2started }
      t2index = if (t2started) 0 else 1
    }
    val t2 = thread {
      Thread.sleep(1)
      this.synchronized { t2started = true }
      val t1s = this.synchronized { t1started }
      t1index = if (t1s) 0 else 1
    }
  
    t1.join()
    t2.join()
    assert(!(t1index == 1 && t2index == 1), s"t1 = $t1index, t2 = $t2index")
  }
}


object SynchronizedNesting extends App {
  import scala.collection._
  class Account(var money: Int)
  val accounts = mutable.HashMap(
    "John" -> new Account(100),
    "Jane" -> new Account(200)
  )
  def add(name: String, money: Int) = accounts.synchronized {
    accounts(name) = new Account(money)
  }
  def removePositive(name: String) = accounts.synchronized {
    accounts.get(name) match {
      case None =>
      case Some(account) =>
        account.synchronized {
          if (account.money >= 0) accounts.remove(name)
        }
    }
  }
  val t1 = thread { add("Jim", 10) }
  val t2 = thread { removePositive("Jane") }
  t1.join()
  t2.join()
  log(s"accounts - $accounts")
}


object SynchronizedDeadlock extends App {
  import SynchronizedNesting._
  def transfer(a1: Account, a2: Account, n: Int) = a1.synchronized {
    a2.synchronized {
      a1.money -= n
      a2.money += n
    }
  }
  val a = new Account(1000)
  val b = new Account(2000)
  val t1 = thread { for (i <- 0 until 100) transfer(a, b, 1) }
  val t2 = thread { for (i <- 0 until 100) transfer(b, a, 1) }
  t1.join()
  t2.join()
  println(s"a = $a, b = $b")
}


object SynchronizedNoDeadlock extends App {
  import SynchronizedProtectedUid._
  class Account(var money: Int) {
    val uid = getUniqueId()
  }
  def transfer(a1: Account, a2: Account, n: Int) {
    def adjust() {
      a1.money -= n
      a2.money += n
    }
    if (a1.uid < a2.uid)
      a1.synchronized { a2.synchronized { adjust() } }
    else
      a2.synchronized { a1.synchronized { adjust() } }
  }
  val a = new Account(1000)
  val b = new Account(2000)
  val t1 = thread { for (i <- 0 until 100) transfer(a, b, 1) }
  val t2 = thread { for (i <- 0 until 100) transfer(b, a, 1) }
  t1.join()
  t2.join()
  println(s"a = $a, b = $b")
}


object SynchronizedDuplicates extends App {
  import scala.collection._
  val duplicates = mutable.Set[Int]()
  val numbers = mutable.ArrayBuffer[Int]()
  def isDuplicate(n: Int): Unit = duplicates.synchronized {
    duplicates.contains(n)
  }
  def addDuplicate(n: Int): Unit = duplicates.synchronized {
    duplicates += n
  }
  def addNumber(n: Int): Unit = numbers.synchronized {
    numbers += n
    if (numbers.filter(_ == n).size > 1) addDuplicate(n)
  }
  val threads = for (i <- 1 to 2) yield thread {
    for (n <- 0 until i * 10) addNumber(n)
  }
  for (t <- threads) t.join()
  println(duplicates.mkString("\n"))
}


object SynchronizedBadPool extends App {
  import scala.collection._
  private val tasks = mutable.Queue[() => Unit]()
  val worker = new Thread {
    override def run() = while (true) tasks.synchronized {
      if (tasks.nonEmpty) {
        val task = tasks.dequeue()
        task()
      }
    }
  }
  worker.setDaemon(true)
  worker.start()

  def asynchronous(body: =>Unit) = tasks.synchronized {
    tasks.enqueue(() => body)
  }

  asynchronous { log("Hello") }
  asynchronous { log(" world!")}
}


object SynchronizedMonitors extends App {
  var message: Option[String] = None
  val greeter = thread {
    this.synchronized {
      while (message == None) this.wait()
      log(message.get)
    }
  }
  this.synchronized {
    message = Some("Hello!")
    this.notify()
  }
  greeter.join()
}


object SynchronizedPool {
  import scala.collection._
  private val tasks = mutable.Queue[() => Unit]()
  val worker = new Thread {
    override def run() = while (true) tasks.synchronized {
      while (tasks.isEmpty) tasks.wait()
      val task = tasks.dequeue()
      task()
    }
  }
  worker.setDaemon(true)
  worker.start()

  def asynchronous(body: =>Unit) = tasks.synchronized {
    tasks.enqueue(() => body)
    tasks.notify()
  }

  def main(args: Array[String]): Unit = {
    asynchronous { log("Hello ") }
    asynchronous { log("World!") }
  }
}





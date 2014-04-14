package org.learningconcurrency
package ch2






object SynchronizedProtectedUid extends App {

  var uidCount = 0L

  def getUniqueId() = this.synchronized {
    val freshUid = uidCount + 1
    uidCount = freshUid
    freshUid
  }

  def getUniqueIds(n: Int): Unit = {
    val uids = for (i <- 0 until n) yield getUniqueId()
    println(s"Generated uids: $uids")
  }

  val t = thread{
    getUniqueIds(5)
  }
  getUniqueIds(5)
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
  val duplicates = mutable.ArrayBuffer[Int]()
  val numbers = mutable.ArrayBuffer[Int]()
  def addDuplicates(n: Int): Unit = {
    if (numbers.filter(_ == n).size > 1) duplicates.synchronized {
      duplicates += n
    }
  }
  val threads = for (i <- 1 to 2) yield thread {
    for (n <- 0 until i * 10) numbers.synchronized {
      numbers += n
      addDuplicates(n)
    }
  }
  for (t <- threads) t.join()
  println(duplicates.mkString("\n"))
}


object SynchronizedDeadlock extends App {
  case class Num(var n: Int)
  def swap(n1: Num, n2: Num) = n1.synchronized {
    n2.synchronized {
      val tmp = n1.n
      n1.n = n2.n
      n2.n = tmp
    }
  }
  val a = Num(1)
  val b = Num(2)
  val t1 = thread { for (i <- 0 until 100) swap(a, b) }
  val t2 = thread { for (i <- 0 until 100) swap(b, a) }
  t1.join()
  t2.join()
  println(s"a = $a, b = $b")
}


object SynchronizedOrders extends App {
  import scala.collection._
  case class Delivery(name: String)
  val orders = mutable.ArrayBuffer[String]()
  val deliveries = mutable.ArrayBuffer[Delivery]()
  val requests = for (i <- 0 until 3) yield thread {
    for (k <- 0 until 100) orders.synchronized {
      orders += s"Item-$i-$k"
    }
  }
  val worker = thread {
    var total = 0
    while (total < 300) {
      orders.synchronized {
        if (orders.nonEmpty) deliveries.synchronized {
          deliveries += Delivery(orders.last)
          orders.remove(orders.length - 1)
          total += 1
        }
      }
    }
  }
  for (r <- requests) r.join()
  worker.join()
  println(deliveries.mkString("\n"))
  println(s"Total deliveries: ${deliveries.size}")
  println(s"Total orders left: ${orders.size}")
}


object SynchronizedMonitors extends App {
  var message: Option[String] = None
  val greeter = thread {
    this.synchronized {
      while (message == None) this.wait()
      println(message.get)
    }
  }
  this.synchronized {
    message = Some("Hello!")
    this.notify()
  }
  greeter.join()
}


object SynchronizedPool {
  private val tasks = scala.collection.mutable.Queue[() => Unit]()
  val worker = new Thread {
    override def run() = while (true) tasks.synchronized {
      while (tasks.isEmpty) tasks.wait()
      val task = tasks.dequeue()
      task()
      run()
    }
  }
  worker.setDaemon(true)
  worker.start()

  def asynchronous(body: =>Unit) = tasks.synchronized {
    tasks.enqueue(() => body)
    tasks.notify()
  }

  def main(args: Array[String]): Unit = {
    asynchronous { println("Hello ") }
    asynchronous { println("World!") }
  }
}





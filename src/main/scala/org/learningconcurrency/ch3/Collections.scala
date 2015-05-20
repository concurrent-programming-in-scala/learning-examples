package org.learningconcurrency
package ch3






object CollectionsBad extends App {
  import scala.collection._

  val buffer = mutable.ArrayBuffer[Int]()

  def add(numbers: Seq[Int]) = execute {
    buffer ++= numbers
    log(s"buffer = $buffer")
  }

  add(0 until 10)
  add(10 until 20)
}


/*
// This example is not in the book.
object CollectionsSynchronized extends App {
  import scala.collection._

  val buffer = new mutable.BufferProxy[Int] with mutable.SynchronizedBuffer[Int] {
    val self = mutable.ArrayBuffer[Int]()
  }

  execute {
    buffer ++= (0 until 10)
    log(s"buffer = $buffer")
  }

  execute {
    buffer ++= (10 until 20)
    log(s"buffer = $buffer")
  }

}
*/


object MiscSyncVars extends App {
  import scala.concurrent._
  val sv = new SyncVar[String]

  execute {
    Thread.sleep(500)
    log("sending a message")
    sv.put("This is secret.")
  }

  log(s"get  = ${sv.get}")
  log(s"take = ${sv.take()}")

  execute {
    Thread.sleep(500)
    log("sending another message")
    sv.put("Secrets should not be logged!")
  }

  log(s"take = ${sv.take()}")
  log(s"take = ${sv.take(timeout = 1000)}")
}


object MiscDynamicVars extends App {
  import scala.util.DynamicVariable

  val dynlog = new DynamicVariable[String => Unit](log)
  def secretLog(msg: String) = println(s"(unknown thread): $msg")

  execute {
    dynlog.value("Starting asynchronous execution.")
    dynlog.withValue(secretLog) {
      dynlog.value("Nobody knows who I am.")
    }
    dynlog.value("Ending asynchronous execution.")
  }

  dynlog.value("is calling the log method!")
}


object CollectionsIterators extends App {
  import java.util.concurrent._

  val queue = new LinkedBlockingQueue[String]
  for (i <- 1 to 5500) queue.offer(i.toString)
  execute {
    val it = queue.iterator
    while (it.hasNext) log(it.next())
  }
  for (i <- 1 to 5500) queue.poll()
}


object CollectionsConcurrentMap extends App {
  import java.util.concurrent.ConcurrentHashMap
  import scala.collection._
  import scala.collection.convert.decorateAsScala._
  import scala.annotation.tailrec

  val emails = new ConcurrentHashMap[String, List[String]]().asScala

  execute {
    emails("James Gosling") = List("james@javalove.com")
    log(s"emails = $emails")
  }

  execute {
    emails.putIfAbsent("Alexey Pajitnov", List("alexey@tetris.com"))
    log(s"emails = $emails")
  }

  execute {
    emails.putIfAbsent("Alexey Pajitnov", List("alexey@welltris.com"))
    log(s"emails = $emails")
  }

}


object CollectionsConcurrentMapIncremental extends App {
  import java.util.concurrent.ConcurrentHashMap
  import scala.collection._
  import scala.collection.convert.decorateAsScala._
  import scala.annotation.tailrec

  val emails = new ConcurrentHashMap[String, List[String]]().asScala

  @tailrec def addEmail(name: String, address: String) {
    emails.get(name) match {
      case Some(existing) =>
        if (!emails.replace(name, existing, address :: existing)) addEmail(name, address)
      case None =>
        if (emails.putIfAbsent(name, address :: Nil) != None) addEmail(name, address)
    }
  }

  execute {
    addEmail("Yukihiro Matsumoto", "ym@ruby.com")
    log(s"emails = $emails")
  }

  execute {
    addEmail("Yukihiro Matsumoto", "ym@ruby.io")
    log(s"emails = $emails")
  }

}


object CollectionsConcurrentMapBulk extends App {
  import scala.collection._
  import scala.collection.convert.decorateAsScala._
  import java.util.concurrent.ConcurrentHashMap

  val names = new ConcurrentHashMap[String, Int]().asScala
  names("Johnny") = 0
  names("Jane") = 0
  names("Jack") = 0

  execute {
    for (n <- 0 until 10) names(s"John $n") = n
  }

  execute {
    for (n <- names) log(s"name: $n")
  }

}


object CollectionsTrieMapBulk extends App {
  import scala.collection._

  val names = new concurrent.TrieMap[String, Int]
  names("Janice") = 0
  names("Jackie") = 0
  names("Jill") = 0

  execute {
    for (n <- 10 until 100) names(s"John $n") = n
  }

  execute {
    log("snapshot time!")
    for (n <- names.map(_._1).toSeq.sorted) log(s"name: $n")
  }

}






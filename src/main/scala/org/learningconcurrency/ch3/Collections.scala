package org.learningconcurrency
package ch3






object CollectionsBad extends App {
  import scala.collection._

  val buffer = mutable.ArrayBuffer[Int]()

  execute(runnable {
    buffer ++= (0 until 10)
    log(s"buffer = $buffer")
  })

  execute(runnable {
    buffer ++= (10 until 20)
    log(s"buffer = $buffer")
  })
}


object CollectionsSynchronized extends App {
  import scala.collection._

  val buffer = new mutable.BufferProxy[Int] with mutable.SynchronizedBuffer[Int] {
    val self = mutable.ArrayBuffer[Int]()
  }

  execute(runnable {
    buffer ++= (0 until 10)
    log(s"buffer = $buffer")
  })

  execute(runnable {
    buffer ++= (10 until 20)
    log(s"buffer = $buffer")
  })

}


object MiscSyncVars extends App {
  import scala.concurrent._
  val sv = new SyncVar[String]

  execute(runnable {
    Thread.sleep(500)
    log("sending a message")
    sv.put("This is secret.")
  })

  log(s"get  = ${sv.get}")
  log(s"take = ${sv.take()}")

  execute(runnable {
    Thread.sleep(500)
    log("sending another message")
    sv.put("Secrets should not be logged!")
  })

  log(s"take = ${sv.take()}")
  log(s"take = ${sv.take(timeout = 1000)}")
}


object MiscDynamicVars extends App {
  import scala.util.DynamicVariable

  val dynlog = new DynamicVariable[String => Unit](log)
  def secretLog(msg: String) = println(s"(unknown thread): $msg")

  execute(runnable {
    dynlog.value("Starting asynchronous execution.")
    dynlog.withValue(secretLog) {
      dynlog.value("Nobody knows who I am.")
    }
    dynlog.value("Ending asynchronous execution.")
  })

  dynlog.value("is calling the log method!")
}


object CollectionsConcurrentMap extends App {
  import java.util.concurrent.ConcurrentHashMap
  import scala.collection._
  import scala.collection.convert.decorateAsScala._
  import scala.annotation.tailrec

  val emails = new ConcurrentHashMap[String, List[String]]().asScala

  execute(runnable {
    emails("James Gosling") = List("james@lovejava.com")
    log(s"emails = $emails")
  })

  execute(runnable {
    emails.putIfAbsent("Alexey Pajitnov", List("alexey@tetris.com"))
    log(s"emails = $emails")
  })

  execute(runnable {
    emails.putIfAbsent("Alexey Pajitnov", List("alexey@welltris.com"))
    log(s"emails = $emails")
  })

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

  execute(runnable {
    addEmail("Yukihiro Matsumoto", "ym@ruby.com")
    log(s"emails = $emails")
  })

  execute(runnable {
    addEmail("Yukihiro Matsumoto", "ym@ruby.io")
    log(s"emails = $emails")
  })

}


object CollectionsConcurrentMapBulk extends App {
  import scala.collection._
  import scala.collection.convert.decorateAsScala._
  import java.util.concurrent.ConcurrentHashMap

  val names = new ConcurrentHashMap[String, String]().asScala
  names("John") = "Doe"
  names("Jane") = "Doe"
  names("Jack") = "Daniels"

  execute(runnable {
    for (n <- 0 until 10) names(s"John $n") = ", of Scotland"
  })

  execute(runnable {
    for (n <- names) log(s"name: $n")
  })

}


object CollectionsTrieMap extends App {
  import scala.collection._

  val names = new concurrent.TrieMap[String, String]
  names("Janice") = "Joplin"
  names("Jackie") = "Chan"
  names("Jill") = "of the Jungle"

  execute(runnable {
    for (n <- 0 until 100) names(s"John $n") = s", $n. Duke of Scotland"
  })

  execute(runnable {
    log("snapshot time!")
    for (n <- names) log(s"Found name: $n")
  })

}






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
  names("Jimmy") = "Wales"

  execute(runnable {

  })

  execute(runnable {
    //for (n <- names) 
  })

}


object CollectionsConcurrentSet extends App {

}


object CollectionsConcurrentQueue extends App {

}


object CollectionsConcurrentTraversal extends App {

}


object CollectionsTrieMap extends App {

}


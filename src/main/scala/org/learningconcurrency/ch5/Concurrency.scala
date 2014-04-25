package org.learningconcurrency
package ch5






object ConcurrencyCollections extends App {
  import java.util.concurrent.ConcurrentSkipListSet
  import scala.collection._
  import scala.collection.JavaConverters._
  import scala.concurrent.ExecutionContext.Implicits.global
  import ParHtmlSpecSearch.getHtmlSpec
  import ch4.FuturesCallbacks.getUrlSpec

  def intersection(a: GenSet[String], b: GenSet[String]): GenSet[String] = {
    val result = new ConcurrentSkipListSet[String]
    for (x <- a.par) if (b contains x) result.add(x)
    result.asScala
  }

  val ifut = for {
    htmlSpec <- getHtmlSpec()
    urlSpec <- getUrlSpec()
  } yield {
    val htmlWords = htmlSpec.mkString.split("\\s+").toSet
    val urlWords = urlSpec.mkString.split("\\s+").toSet
    intersection(htmlWords, urlWords)
  }

  ifut onSuccess { case i =>
    log(s"intersection = $i")
  }

}


object ConcurrencyCollectionsBad extends App {
  import java.util.concurrent.ConcurrentSkipListSet
  import scala.collection._
  import scala.collection.parallel._

  def toPar[T](c: ConcurrentSkipListSet[T]): ParSet[T] = ???

  val c = new ConcurrentSkipListSet[Int]
  for (i <- 0 until 100) c.add(i)
  
  for (x <- toPar(c)) c.add(x) // bad
}


object ConcurrencyTrieMap extends App {
  import scala.collection._

  val cache = new concurrent.TrieMap[Int, String]()
  for (i <- 0 until 100) cache(i) = i.toString

  for ((number, string) <- cache.par) cache(-number) = s"-$string"

  log(s"cache - ${cache.keys.toList.sorted}")
}




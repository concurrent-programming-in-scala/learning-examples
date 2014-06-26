package org.learningconcurrency
package ch6






object CompositionMapAndFilter extends App {
  import rx.lang.scala._
  import scala.concurrent.duration._

  val odds = Observable.interval(0.5 seconds).filter(_ % 2 == 1).map(n => s"odd number $n").take(5)
  odds.subscribe(log _, e => log(s"unexpected $e"), () => log("no more odds"))

  val evens = for (n <- Observable.from(0 until 9); if n % 2 == 0) yield s"even number $n"
  evens.subscribe(log _)

}


object CompositionConcatAndFlatten extends App {
  import rx.lang.scala._
  import scala.concurrent._
  import scala.concurrent.duration._
  import ExecutionContext.Implicits.global
  import scala.io.Source

  def fetchQuote(): Future[String] = Future {
    blocking {
      val url = "http://www.iheartquotes.com/api/v1/random?show_permalink=false&show_source=false"
      Source.fromURL(url).getLines.mkString
    }
  }

  def fetchQuoteObservable(): Observable[String] = Observable.from(fetchQuote())

  def quotes: Observable[Observable[String]] = Observable.interval(0.5 seconds).take(5).map {
    n => fetchQuoteObservable().map(txt => s"$n) $txt")
  }
  
  log(s"Using concat")
  quotes.concat.subscribe(log _)

  Thread.sleep(6000)

  log(s"Now using flatten")
  quotes.flatten.subscribe(log _)

  Thread.sleep(6000)

}


// There is always one more bug.
object CompositionRetry extends App {
  import rx.lang.scala._
  import scala.concurrent.duration._
  import scala.io.Source

  def quote = Observable[String] { subscriber =>
    val url = "http://www.iheartquotes.com/api/v1/random?show_permalink=false&show_source=false"
    val text = Source.fromURL(url).getLines.mkString
    if (text.size > 100) {
      subscriber.onNext("<quote too long>")
      subscriber.onError(new Exception("Too long"))
    } else {
      subscriber.onNext(text.mkString)
      subscriber.onCompleted()
    }
  }

  quote.retry.take(3).subscribe(log _, e => log(s"cannot find quote - $e"), () => log("done!"))

}


object CompositionReduce extends App {
  import rx.lang.scala._
  import scala.concurrent.duration._

  def shortQuote = CompositionRetry.quote.retry.take(5).filter(_ != "<quote too long>")
  val shortQuotesCollection = (shortQuote ++ shortQuote ++ shortQuote).foldLeft("") { (acc, q) =>
    s"$acc$q\n\n"
  }

  shortQuotesCollection.subscribe(log _)

}


object CompositionErrors extends App {
  import rx.lang.scala._

  val status = Observable[String] { sub =>
    sub.onNext("ok")
    sub.onNext("still ok")
    sub.onError(new Exception("very bad"))
  }

  val fixedStatus = status.onErrorReturn(e => e.getMessage)
  fixedStatus.subscribe(log _)

  val continuedStatus = status.onErrorResumeNext(e => Observable.items("better", "much better"))
  continuedStatus.subscribe(log _)

}







package org.learningconcurrency
package ch6






object CompositionMapAndFilter extends App {
  import rx.lang.scala._
  import scala.concurrent.duration._

  val odds = Observable.interval(0.5.seconds).filter(_ % 2 == 1).map(n => s"odd number $n").take(5)
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

  def quotes: Observable[Observable[String]] = Observable.interval(0.5.seconds).take(5).map {
    n => fetchQuoteObservable().map(txt => s"$n) $txt")
  }
  
  log(s"Using concat")
  quotes.concat.subscribe(log _)

  Thread.sleep(6000)

  log(s"Now using flatten")
  quotes.flatten.subscribe(log _)

  Thread.sleep(6000)

  log(s"Now using flatMap")
  Observable.interval(0.5.seconds).take(5).flatMap({
    n => fetchQuoteObservable().map(txt => s"$n) $txt")
  }).subscribe(log _)

  Thread.sleep(6000)

  log(s"Now using good ol' for-comprehensions")
  val qs = for {
    n   <- Observable.interval(0.5.seconds).take(5)
    txt <- fetchQuoteObservable()
  } yield s"$n) $txt"
  qs.subscribe(log _)

  Thread.sleep(6000)

}


// There is always one more bug.
object CompositionRetry extends App {
  import rx.lang.scala._
  import scala.concurrent.duration._
  import scala.io.Source
  import Observable._

  def randomQuote = Observable.apply[String] { obs =>
    val url = "http://www.iheartquotes.com/api/v1/random?" +
      "show_permalink=false&show_source=false"
    obs.onNext(Source.fromURL(url).getLines.mkString)
    obs.onCompleted()
    Subscription()
  }

  def errorMessage = items("Retrying...") ++ error(new Exception)
  
  def shortQuote = for {
    txt     <- randomQuote
    message <- if (txt.length < 100) items(txt) else errorMessage
  } yield message

  shortQuote.retry(5).subscribe(log _, e => log(s"too long - $e"), () => log("done!"))

}


object CompositionScan extends App {
  import rx.lang.scala._

  CompositionRetry.shortQuote.retry.repeat.take(100).scan(0) {
    (n, q) => if (q == "Retrying...") n + 1 else n
  } subscribe(n => log(s"$n / 100"))
}


object CompositionReduce extends App {
  import rx.lang.scala._
  import scala.concurrent.duration._

  def shortQuote = CompositionRetry.randomQuote.retry.take(5).filter(_ != "Retrying...")
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







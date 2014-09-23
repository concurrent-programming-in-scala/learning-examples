package org.learningconcurrency
package ch6






object SchedulersComputation extends App {
  import rx.lang.scala._

  val scheduler = schedulers.ComputationScheduler()
  val numbers = Observable.from(0 until 20)
  numbers.subscribe(n => log(s"num $n"))
  numbers.observeOn(scheduler).subscribe(n => log(s"num $n"))

}


object SchedulersSwing extends scala.swing.SimpleSwingApplication {
  import rx.lang.scala._
  import scala.swing._
  import scala.swing.event._

  def top = new MainFrame {
    title = "Swing Observables"

    val button = new Button {
      text = "Click"
    }

    contents = button

    val buttonClicks = Observable[Unit] { sub =>
      button.reactions += {
        case ButtonClicked(_) => sub.onNext(())
      }
    }

    buttonClicks.subscribe(_ => log("button clicked"))
  }

}


object SchedulersBrowser extends scala.swing.SimpleSwingApplication {
  import rx.lang.scala._
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  import scala.swing._
  import scala.swing.event._
  import scala.io.Source
  import java.util.concurrent.Executor

  abstract class BrowserFrame extends MainFrame {
    title = "MiniBrowser"

    val termfield = new TextField("http://www.w3.org/Addressing/URL/url-spec.txt")
    val pagefield = new TextArea
    val button = new Button {
      text = "Feeling Lucky"
    }

    contents = new BorderPanel {
      import BorderPanel.Position._
      layout(new BorderPanel {
        layout(new Label("URL:")) = West
        layout(termfield) = Center
        layout(button) = East
      }) = North
      layout(pagefield) = Center
    }

    size = new Dimension(1024, 768)
  }

  trait BrowserLogic {
    self: BrowserFrame =>
    
    def suggestRequest(term: String): Observable[String] = {
      val url = s"http://suggestqueries.google.com/complete/search?client=firefox&q=$term"
      val request = Future { Source.fromURL(url).mkString }
      Observable.from(request).timeout(0.5.seconds).onErrorResumeNext(Observable.items("(no suggestions)"))
    }

    def pageRequest(url: String): Observable[String] = {
      val request = Future { Source.fromURL(url).mkString }
      Observable.from(request).timeout(4.seconds).onErrorResumeNext(t => Observable.items(s"Could not load page: $t"))
    }

    termfield.texts.map(suggestRequest).concat.observeOn(swingScheduler).subscribe {
      response => pagefield.text = response
    }

    button.clicks.map(_ => pageRequest(termfield.text)).concat.observeOn(swingScheduler).subscribe {
      response => pagefield.text = response
    }
  }

  def top = new BrowserFrame with BrowserLogic

}


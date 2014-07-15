package org.learningconcurrency



import scala.swing._
import scala.swing.event._
import javax.swing._
import java.awt.event._
import rx.lang.scala._
import java.util.concurrent.Executor



package object ch6 {

  implicit class ButtonOps(val self: Button) {
    def clicks = Observable[Unit] { sub =>
      self.reactions += {
        case ButtonClicked(_) => sub.onNext(())
      }
    }
  }
  
  implicit class TextFieldOps(val self: TextField) {
    def texts = Observable[String] { sub =>
      self.reactions += {
        case ValueChanged(_) => sub.onNext(self.text)
      }
    }
  }

  implicit class TableOps(val self: Table) {
    def rowDoubleClicks = Observable[Int] { sub =>
      self.peer.addMouseListener(new MouseAdapter {
        override def mouseClicked(e: java.awt.event.MouseEvent) {
          if (e.getClickCount == 2) {
            val row = self.peer.getSelectedRow
            sub.onNext(row)
          }
        }
      })
    }
  }

  def swing(body: =>Unit) = {
    val r = new Runnable {
      def run() = body
    }
    javax.swing.SwingUtilities.invokeLater(r)
  }

  val swingScheduler = new Scheduler {
    val asJavaScheduler = rx.schedulers.Schedulers.from(new Executor {
      def execute(r: Runnable) = javax.swing.SwingUtilities.invokeLater(r)
    })
  }

  
}

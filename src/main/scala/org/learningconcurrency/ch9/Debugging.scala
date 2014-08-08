package org.learningconcurrency
package ch9






object Deadlock extends App {
  
  class Account(var money: Int)

  def send(a: Account, b: Account, n: Int) = a.synchronized {
    b.synchronized {
      a.money -= n
      b.money += n
    }
  }

  val a = new Account(1000)
  val b = new Account(2000)
  val t1 = ch2.thread { for (i <- 0 until 100) send(a, b, 1) }
  val t2 = ch2.thread { for (i <- 0 until 100) send(b, a, 1) }
  t1.join()
  t2.join()

}


object Correctness extends App {
  import java.util.concurrent.atomic._
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.annotation.tailrec

  class Accumulator[T](z: T)(op: (T, T) => T) {
    private val value = new AtomicReference(z)
    @tailrec final def add(v: T): Unit = {
      val ov = value.get
      if (!value.compareAndSet(ov, op(ov, v))) add(v)
    }
    def apply() = value.get
  }

  class CountDownLatch(n: Int)(onDone: =>Unit) {
    private val left = new AtomicInteger(n)
    def count() =
      if (left.decrementAndGet() <= 1) onDone
  }

  def fold[T](fs: Seq[Future[T]])(z: T)(op: (T, T) => T): Future[T] = {
    val p = Promise[T]()
    val accu = new Accumulator(z)(op)
    val latch = new CountDownLatch(fs.length)(p.trySuccess(accu()))
    for (f <- fs) f onSuccess { case v =>
      accu.add(v)
      latch.count()
    }
    p.future
  }

  val fs = for (i <- 0 until 5) yield Future { i }
  val folded = fold(fs)(0)(_ + _)
  folded onSuccess { case v => log(s"folded: $v") }

}


object Performance extends App {



}


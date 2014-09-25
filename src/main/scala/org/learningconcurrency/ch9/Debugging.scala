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
      val nv = op(ov, v)
      if (!value.compareAndSet(ov, nv)) add(v)
    }
    def apply() = value.get
  }

  class CountDownLatch(n: Int)(onDone: =>Unit) {
    private val left = new AtomicInteger(n)
    def count() =
      if (left.decrementAndGet() == 0) onDone
  }

  def fold[T](fs: Seq[Future[T]])(z: T)(op: (T, T) => T): Future[T] = {
    val p = Promise[T]()
    val accu = new Accumulator(z)(op)
    val latch = new CountDownLatch(fs.length)({
      val total = accu()
      p.trySuccess(total)
    })
    for (f <- fs) f foreach { case v =>
      accu.add(v)
      latch.count()
    }
    p.future
  }

  val fs = for (i <- 0 until 5) yield Future { i }
  val folded = fold(fs)(0)(_ + _)
  folded foreach { case v => log(s"folded: $v") }

}


object Performance extends App {
  import java.util.concurrent.atomic._
  import scala.annotation.tailrec
  import org.scalameter._

  class Accumulator[T](z: T)(op: (T, T) => T) {
    private val value = new AtomicReference(z)
    @tailrec final def add(v: T): Unit = {
      val ov = value.get
      val nv = op(ov, v)
      if (!value.compareAndSet(ov, nv)) add(v)
    }
    def apply() = value.get
  }

  val time = measure {
    val acc = new Accumulator(0L)(_ + _)
    var i = 0
    val total = 1000000
    while (i < total) {
      acc.add(i)
      i += 1
    }
  }

  println("Running time: " + time)

  val accTime = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 30,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default) measure {
    val acc = new Accumulator(0L)(_ + _)
    var i = 0
    val total = 1000000
    while (i < total) {
      acc.add(i)
      i += 1
    }
  }

  println("Accumulator time: " + accTime)

  class LongAccumulator(z: Long)(op: (Long, Long) => Long) {
    private val value = new AtomicLong(z)
    @tailrec final def add(v: Long): Unit = {
      val ov = value.get
      val nv = op(ov, v)
      if (!value.compareAndSet(ov, nv)) add(v)
    }
    def apply() = value.get
  }

  val longAccTime = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 30,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default) measure {
    val acc = new LongAccumulator(0L)(_ + _)
    var i = 0
    val total = 1000000
    while (i < total) {
      acc.add(i)
      i += 1
    }
  }

  println("Long accumulator time: " + longAccTime)

  val longAccTime4 = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 30,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default) measure {
    val acc = new LongAccumulator(0L)(_ + _)
    val total = 1000000
    val p = 4
    val threads = for (j <- 0 until p) yield ch2.thread {
      val start = j * total / p
      var i = start
      while (i < start + total / p) {
        acc.add(i)
        i += 1
      }
    }
    for (t <- threads) t.join()
  }

  println("4 threads long accumulator time: " + longAccTime4)

  class ParLongAccumulator(z: Long)(op: (Long, Long) => Long) {
    private val par = Runtime.getRuntime.availableProcessors * 128
    private val values = new AtomicLongArray(par)
    @tailrec final def add(v: Long): Unit = {
      val id = Thread.currentThread.getId.toInt
      val i = math.abs(scala.util.hashing.byteswap32(id)) % par
      val ov = values.get(i)
      val nv = op(ov, v)
      if (!values.compareAndSet(i, ov, nv)) add(v)
    }
    def apply(): Long = {
      var total = 0L
      for (i <- 0 until values.length) total = op(total, values.get(i))
      total
    }
  }

  val parLongAccTime = config(
    Key.exec.minWarmupRuns -> 100,
    Key.exec.maxWarmupRuns -> 200,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default) measure {
    val acc = new ParLongAccumulator(0L)(_ + _)
    val total = 1000000
    val p = 4
    val threads = for (j <- 0 until p) yield ch2.thread {
      val start = j * total / p
      var i = start
      while (i < start + total / p) {
        acc.add(i)
        i += 1
      }
    }
    for (t <- threads) t.join()
  }

  println("Parallel long accumulator time: " + parLongAccTime)

}


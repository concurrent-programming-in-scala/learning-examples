package org.learningconcurrency
package ch3



import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec



object LockFreePool {
  class Pool[T] {
    val parallelism = Runtime.getRuntime.availableProcessors * 32
    val buckets = new Array[AtomicReference[(List[T], Long)]](parallelism)
    for (i <- 0 until buckets.length)
      buckets(i) = new AtomicReference((Nil, 0L))

    def add(x: T): Unit = {
      val i = (Thread.currentThread.getId * x.## % buckets.length).toInt
      @tailrec def retry() {
        val bucket = buckets(i)
        val v = bucket.get
        val (lst, stamp) = v
        val nlst = x :: lst
        val nstamp = stamp + 1
        val nv = (nlst, nstamp)
        if (!bucket.compareAndSet(v, nv)) retry()
      }
      retry()
    }

    def remove(): Option[T] = {
      val start = (Thread.currentThread.getId % buckets.length).toInt
      @tailrec def scan(witness: Long): Option[T] = {
        var i = (start + 1) % buckets.length
        var sum = 0L
        while (i != start) {
          val bucket = buckets(i)

          @tailrec def retry(): Option[T] = {
            bucket.get match {
              case (Nil, stamp) =>
                sum += stamp
                None
              case v @ (lst, stamp) =>
                val nv = (lst.tail, stamp + 1)
                if (bucket.compareAndSet(v, nv)) Some(lst.head)
                else retry()
            }
          }
          retry() match {
            case Some(v) => return Some(v)
            case None =>
          }

          i = (i + 1) % buckets.length
        }
        if (sum == witness) None
        else scan(sum)
      }
      scan(-1L)
    }
  }

  def main(args: Array[String]) {
    val check = new ConcurrentHashMap[Int, Unit]()
    val pool = new Pool[Int]
    val p = 8
    val num = 1000000
    val inserters = for (i <- 0 until p) yield ch2.thread {
      for (j <- 0 until num) pool.add(i * num + j)
    }
    inserters.foreach(_.join())
    val removers = for (i <- 0 until p) yield ch2.thread {
      for (j <- 0 until num) {
        pool.remove() match {
          case Some(v) => check.put(v, ())
          case None => sys.error("Should be non-empty.")
        }
      }
    }
    removers.foreach(_.join())
    for (i <- 0 until (num * p)) assert(check.containsKey(i))
  }
}

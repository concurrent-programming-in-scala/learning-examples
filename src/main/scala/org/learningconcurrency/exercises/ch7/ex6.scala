package org.learningconcurrency
package exercises
package ch7

/**
 * Use ScalaSTM to implement a thread-safe TArrayBuffer class, which extends the scala.collection.mutable.Buffer interface.
 */
object Ex6 extends App {
  import scala.concurrent.stm._

  class TArrayBuffer[T] extends scala.collection.mutable.Buffer[T] {
    private[this] val buf: Ref[Vector[T]] = Ref(Vector.empty[T]) // should store the immutable object in order to use CAS operations.

    @annotation.tailrec
    final def +=(elem: T): this.type = {
      val ts = buf.single()
      if (buf.single.compareAndSet(ts, ts :+ elem))
        this
      else
        +=(elem)
    }

    @annotation.tailrec
    final def +=:(elem: T): this.type = {
      val ts = buf.single()
      if (buf.single.compareAndSet(ts, elem +: ts))
        this
      else
        +=:(elem)
    }

    def apply(n: Int): T = buf.single()(n)

    @annotation.tailrec
    final def clear(): Unit = {
      val ts = buf.single()
      if (!buf.single.compareAndSet(ts, Vector.empty[T]))
        clear()
    }

    @annotation.tailrec
    final def insertAll(n: Int, elems: collection.Traversable[T]): Unit = {
      val ts = buf.single()
      val len = ts.length
      if (n < 0 || n > len) throw new IndexOutOfBoundsException(n.toString)

      val left = ts.take(n)
      val right = ts.drop(n)
      val nts = left ++ elems ++ right
      if (!buf.single.compareAndSet(ts, nts))
        insertAll(n, elems)
    }

    def length: Int = buf.single().length

    @annotation.tailrec
    final def remove(n: Int): T = {
      val ts = buf.single()
      val len = ts.length
      if (n < 0 || n >= len) throw new IndexOutOfBoundsException(n.toString)

      val left = ts.take(n)
      val right = ts.drop(n + 1)
      val nts = left ++ right
      if (buf.single.compareAndSet(ts, nts))
        ts(n)
      else
        remove(n)
    }

    @annotation.tailrec
    final def update(n: Int, newelem: T): Unit = {
      val ts = buf.single()
      val len = ts.length
      if (n < 0 || n >= len) throw new IndexOutOfBoundsException(n.toString)

      val nts = ts.updated(n, newelem)
      if (!buf.single.compareAndSet(ts, nts))
        update(n, newelem)
    }

    def iterator: Iterator[T] = buf.single().iterator
  }


  // for `+=`.
  {
    val buf = new TArrayBuffer[Int]()
    buf += 1
    buf += 2
    assert(buf.toList == List(1, 2))
  }

  // for `+=:`.
  {
    val buf = new TArrayBuffer[Int]()
    1 +=: buf
    0 +=: buf
    assert(buf.toList == List(0, 1))
  }

  // for `clear`.
  {
    val buf = new TArrayBuffer[Int]()
    buf += 1
    buf += 2
    buf.clear()
    assert(buf.toList == Nil)
  }

  // for `insertAll`.
  {
    // insert into the index 0.
    val buf1 = new TArrayBuffer[Int]()
    buf1 += 2
    buf1.insertAll(0, List(1))
    assert(buf1.toList == List(1, 2))

    // insert into the index `length`.
    val buf2 = new TArrayBuffer[Int]()
    buf2 += 1
    buf2.insertAll(1, List(2))
    assert(buf2.toList == List(1, 2))

    // insert into any index (0 < n < `length`).
    val buf3 = new TArrayBuffer[Int]()
    buf3 += 1
    buf3 += 5
    buf3.insertAll(1, List(2, 3, 4))
    assert(buf3.toList == List(1, 2, 3, 4, 5))
  }

  // for `remove`.
  {
    val buf = new TArrayBuffer[Int]()
    buf += 1
    buf += 2
    buf += 1
    buf += 3
    val removed = buf.remove(2)
    assert(removed == 1)
    assert(buf.toList == List(1, 2, 3))
  }

  // for `update`.
  {
    val buf = new TArrayBuffer[Int]()
    buf += 1
    buf += 2
    buf(1) = 3
    assert(buf.toList == List(1, 3))
  }

  // for concurrency of `+=`.
  {
    val buf = new TArrayBuffer[Int]()
    val threads = (1 to 10).map(i => new Thread {
      override def run(): Unit = {
        Thread.sleep(15) // tweak not to append `i` in iteration order.
        buf += i
      }
    })
    threads.foreach(_.start())
    threads.foreach(_.join())
    assert(buf.length == 10)
  }

  // for concurrency of `+=:`.
  {
    val buf = new TArrayBuffer[Int]()
    val threads = (1 to 10).map(i => new Thread {
      override def run(): Unit = {
        Thread.sleep(15)
        i +=: buf
      }
    })
    threads.foreach(_.start())
    threads.foreach(_.join())
    assert(buf.length == 10)
  }

  // for concurrency of `insertAll`.
  {
    val buf = new TArrayBuffer[Int]()
    val threads = (1 to 10).map(i => new Thread {
      override def run(): Unit = {
        Thread.sleep(15)
        buf.insertAll(0, List(i, i * 10))
      }
    })
    threads.foreach(_.start())
    threads.foreach(_.join())
    assert(buf.length == 20)
    (0 until 20 by 2).foreach(i => {
      assert(buf(i + 1) == buf(i) * 10, s"${buf(i)}, ${buf(i + 1)}")
    })
  }

  // for concurrency of `remove`.
  {
    val buf = new TArrayBuffer[Int]()
    buf.insertAll(0, 1 to 10)
    val threads = (1 to 10).map(i => new Thread {
      override def run(): Unit = {
        Thread.sleep(15)
        buf.remove(0)
      }
    })
    threads.foreach(_.start())
    threads.foreach(_.join())
    assert(buf.length == 0)
  }

  // for concurrency of `update`.
  {
    val buf = new TArrayBuffer[Int]()
    buf.insertAll(0, 1 to 10)
    val threads = (0 until 10).map(i => new Thread {
      override def run(): Unit = {
        Thread.sleep(15)
        buf(i) = i + 10
      }
    })
    threads.foreach(_.start())
    threads.foreach(_.join())
    assert(buf.toList == (10 until 10 + 10).toList)
  }

}
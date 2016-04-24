package org.learningconcurrency
package exercises
package ch7

/**
 * Use ScalaSTM to implement a thread-safe TArrayBuffer class, which extends the scala.collection.mutable.Buffer interface.
 */
object Ex6 extends App {
  import scala.concurrent.stm._

  /**
   * `Vector` based implementation.
   */
  class TVectorBuffer[T] extends scala.collection.mutable.Buffer[T] {
    private[this] val buf: Ref[Vector[T]] = Ref(Vector.empty[T])

    def +=(elem: T): this.type = atomic { implicit txn =>
      buf() = buf() :+ elem
      this
    }

    def +=:(elem: T): this.type = atomic { implicit txn =>
      buf() = elem +: buf()
      this
    }

    def apply(n: Int): T = buf.single()(n)

    def clear(): Unit = buf.single.set(Vector.empty[T])

    def insertAll(n: Int, elems: collection.Traversable[T]): Unit = atomic { implicit txn =>
      val ts = buf()
      val len = ts.length
      if (n < 0 || n > len) throw new IndexOutOfBoundsException(n.toString)

      val left = ts.take(n)
      val right = ts.drop(n)
      buf() = left ++ elems ++ right
    }

    def length: Int = buf.single().length

    def remove(n: Int): T = atomic { implicit txn =>
      val ts = buf()
      val len = ts.length
      if (n < 0 || n >= len) throw new IndexOutOfBoundsException(n.toString)

      val left = ts.take(n)
      val right = ts.drop(n + 1)
      buf() = left ++ right
      ts(n)
    }

    def update(n: Int, newelem: T): Unit = atomic { implicit txn =>
      val ts = buf()
      val len = ts.length
      if (n < 0 || n >= len) throw new IndexOutOfBoundsException(n.toString)

      buf() = ts.updated(n, newelem)
    }

    def iterator: Iterator[T] = buf.single().iterator
  }

  // `TVectorBuffer` tests.
  {
    // for `+=`.
    {
      val buf = new TVectorBuffer[Int]()
      buf += 1
      buf += 2
      assert(buf.toList == List(1, 2))
    }

    // for `+=:`.
    {
      val buf = new TVectorBuffer[Int]()
      1 +=: buf
      0 +=: buf
      assert(buf.toList == List(0, 1))
    }

    // for `clear`.
    {
      val buf = new TVectorBuffer[Int]()
      buf += 1
      buf += 2
      buf.clear()
      assert(buf.toList == Nil)
    }

    // for `insertAll`.
    {
      // insert into the index 0.
      val buf1 = new TVectorBuffer[Int]()
      buf1 += 2
      buf1.insertAll(0, List(1))
      assert(buf1.toList == List(1, 2))

      // insert into the index `length`.
      val buf2 = new TVectorBuffer[Int]()
      buf2 += 1
      buf2.insertAll(1, List(2))
      assert(buf2.toList == List(1, 2))

      // insert into any index (0 < n < `length`).
      val buf3 = new TVectorBuffer[Int]()
      buf3 += 1
      buf3 += 5
      buf3.insertAll(1, List(2, 3, 4))
      assert(buf3.toList == List(1, 2, 3, 4, 5))
    }

    // for `remove`.
    {
      val buf = new TVectorBuffer[Int]()
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
      val buf = new TVectorBuffer[Int]()
      buf += 1
      buf += 2
      buf(1) = 3
      assert(buf.toList == List(1, 3))
    }

    // for concurrency of `+=`.
    {
      val buf = new TVectorBuffer[Int]()
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
      val buf = new TVectorBuffer[Int]()
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
      val buf = new TVectorBuffer[Int]()
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
      val buf = new TVectorBuffer[Int]()
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
      val buf = new TVectorBuffer[Int]()
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

}
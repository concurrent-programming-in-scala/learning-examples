package org.learningconcurrency
package exercises
package ch7

/**
 * Use ScalaSTM to implement a thread-safe TArrayBuffer class, which extends the scala.collection.mutable.Buffer interface.
 */
object Ex6 extends App {
  import scala.concurrent.stm._

  class TArrayBuffer[T](initialSize: Int = 8)(implicit cm: scala.reflect.ClassTag[T]) extends scala.collection.mutable.Buffer[T] {
    private[this] val len: Ref[Int] = Ref(0)
    private[this] val array: Ref[TArray[T]] = Ref(TArray.ofDim[T](initialSize))
    private[this] var default: T = _

    def +=(elem: T): this.type = atomic { implicit txn =>
      val newLen = len() + 1
      if (newLen > array().length) {
        copyToNewArray(newLen)
      }

      array()(len()) = elem
      len() = newLen
      this
    }

    def +=:(elem: T): this.type = atomic { implicit txn =>
      val newLen = len() + 1
      if (newLen > array().length) {
        copyToNewArray(newLen)
      }

      shiftRight(0, 1, len())
      array()(0) = elem
      len() = newLen
      this
    }

    def apply(n: Int): T = atomic { implicit txn =>
      if (n < 0 || n >= len()) throw new IndexOutOfBoundsException(n.toString)

      val a = array()
      a(n)
    }

    def clear(): Unit = atomic { implicit txn =>
      array() = TArray.ofDim[T](initialSize)
      len() = 0
    }

    def insertAll(n: Int, elems: Traversable[T]): Unit = atomic { implicit txn =>
      if (n < 0 || n > len()) throw new IndexOutOfBoundsException(n.toString)

      val count = elems.size
      val newLen = len() + count
      if (newLen > array().length) {
        copyToNewArray(newLen)
      }

      shiftRight(n, n + count, len() - n)
      val a = array()
      elems.toIterable.zipWithIndex.foreach { case (t, i) => a(n + i) = t }
      len() = newLen
    }

    def length: Int = len.single()

    def remove(n: Int): T = atomic { implicit txn =>
      if (n < 0 || n >= len()) throw new IndexOutOfBoundsException(n.toString)

      val a = array()
      val result = a(n)

      shiftLeft(n + 1, n, len() - n - 1)
      array()(len() - 1) = default
      len() = len() - 1
      result
    }

    def update(n: Int, newelem: T): Unit = atomic { implicit txn =>
      if (n < 0 || n >= len()) throw new IndexOutOfBoundsException(n.toString)

      array()(n) = newelem
    }

    def iterator: Iterator[T] = atomic { implicit txn =>
      val a = array()
      val seq = for (i <- 0 until len()) yield a(i)
      seq.iterator
    }

    def debugString: String = atomic { implicit txn => array().dbgStr }

    private def copyToNewArray(length: Int)(implicit txn: InTxn): Unit = {
      var newLen: Long = array().length * 2 // use this value as `Long` in order to avoid overflow.
      while (length > newLen) {
        newLen = newLen * 2
      }
      if (newLen > Int.MaxValue)
        newLen = Int.MaxValue

      val newArray = TArray.ofDim[T](newLen.toInt)
      array().refs.zipWithIndex.foreach { case (t, i) => newArray(i) = t() }
      array() = newArray
    }

    private def shiftRight(src: Int, dst: Int, count: Int)(implicit txn: InTxn): Unit = {
      val a = array()
      for (i <- count - 1 to 0 by -1) {
        a(dst + i) = a(src + i)
      }
    }

    private def shiftLeft(src: Int, dst: Int, count: Int)(implicit txn: InTxn): Unit = {
      val a = array()
      for (i <- 0 until count) {
        a(dst + i) = a(src + i)
      }
    }
  }

  // TArrayBuffer tests.
  {
    // for `+=`.
    {
      val buf = new TArrayBuffer[Int](1)
      buf += 1
      buf += 2
      buf += 3
      assert(buf(0) == 1)
      assert(buf(1) == 2)
      assert(buf(2) == 3)
      assert(buf.length == 3)
    }

    // for `+=:`
    {
      val buf = new TArrayBuffer[Int](1)
      3 +=: buf
      2 +=: buf
      1 +=: buf
      assert(buf(0) == 1)
      assert(buf(1) == 2)
      assert(buf(2) == 3)
      assert(buf.length == 3)
    }

    // for `insertAll`.
    {
      // insert into the index 0.
      val buf1 = new TArrayBuffer[Int](2)
      buf1 += 2
      buf1 += 3
      buf1.insertAll(0, List(1))
      assert(buf1(0) == 1)
      assert(buf1(1) == 2)
      assert(buf1(2) == 3)
      assert(buf1.length == 3)

      // insert into the index `length`.
      val buf2 = new TArrayBuffer[Int](2)
      buf2 += 1
      buf2.insertAll(1, List(2, 3))
      assert(buf2(0) == 1)
      assert(buf2(1) == 2)
      assert(buf2(2) == 3)
      assert(buf2.length == 3)

      // insert into any index (0 < n < `length`).
      val buf3 = new TArrayBuffer[Int](2)
      buf3 += 1
      buf3 += 4
      buf3.insertAll(1, List(2, 3))
      assert(buf3(0) == 1)
      assert(buf3(1) == 2)
      assert(buf3(2) == 3)
      assert(buf3(3) == 4)
      assert(buf3.length == 4)
    }

    // for `remove`.
    {
      val buf = new TArrayBuffer[Int]()
      buf += 1
      buf += 2
      buf += 3
      val removed = buf.remove(1)
      assert(removed == 2)
      assert(buf(0) == 1)
      assert(buf(1) == 3)
      assert(buf.length == 2)
    }

    // for `update`.
    {
      val buf = new TArrayBuffer[Int]()
      buf += 1
      buf += 2
      buf(0) = 3
      assert(buf(0) == 3)
      assert(buf(1) == 2)
    }

    // for `clear`.
    {
      val buf = new TArrayBuffer[Int]()
      buf += 1
      buf.clear()
      assert(buf.length == 0)
    }

    // for `iterator`.
    {
      val buf = new TArrayBuffer[Int]()
      buf += 1
      buf += 2
      assert(buf.toList == List(1, 2))
    }

    // for concurrency of `+=`.
    {
      val buf = new TArrayBuffer[Int](1)
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
      val buf = new TArrayBuffer[Int](1)
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
      val buf = new TArrayBuffer[Int](1)
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
  }

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
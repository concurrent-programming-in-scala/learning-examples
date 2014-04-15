package org.learningconcurrency
package ch2






object UnsafeUid extends App {
  import scala.annotation.tailrec
  private val unsafe = scala.concurrent.util.Unsafe.instance
  private val uidCountOffset = unsafe.objectFieldOffset(UnsafeUid.getClass.getDeclaredField("uidCount"))
  @volatile var uidCount = 0L

  @tailrec def getUniqueId(): Long = {
    val oldUid = uidCount
    val newUid = uidCount + 1
    if (unsafe.compareAndSwapLong(UnsafeUid, uidCountOffset, oldUid, newUid)) newUid
    else getUniqueId()
  }

  def getUniqueIds(n: Int): Unit = {
    val uids = for (i <- 0 until n) yield getUniqueId()
    log(s"Generated uids: $uids")
  }

  val t = thread {
    getUniqueIds(5)
  }
  getUniqueIds(5)
  t.join()

}



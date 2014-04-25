package org.learningconcurrency
package ch5



import scala.collection.parallel._



class ParString(val str: String) extends immutable.ParSeq[Char] {
  def apply(i: Int) = str.charAt(i)
  
  def length = str.length
  
  def seq = new collection.immutable.WrappedString(str)
  
  def splitter = new ParStringSplitter(str, 0, str.length)
  
  class ParStringSplitter(private var s: String, private var i: Int, private val ntl: Int)
  extends SeqSplitter[Char] {
    final def hasNext = i < ntl
    final def next = {
      val r = s.charAt(i)
      i += 1
      r
    }
    def remaining = ntl - i
    def dup = new ParStringSplitter(s, i, ntl)
    def split = {
      val rem = remaining
      if (rem >= 2) psplit(rem / 2, rem - rem / 2)
      else Seq(this)
    }
    def psplit(sizes: Int*): Seq[ParStringSplitter] = {
      for (sz <- sizes) yield {
        val next = (i + sz) min ntl
        val ps = new ParStringSplitter(s, i, next)
        i = next
        ps
      }
    }
  }
  
}


object CustomCharCount extends App {
  val txt = "A custom text " * 250000
  val partxt = new ParString(txt)
  
  val seqtime = warmedTimed(50) {
    txt.foldLeft(0)((x, y) => x + 1)
  }
  
  log(s"Sequential time - $seqtime ms")

  val partime = warmedTimed(50) {
    partxt.aggregate(0)((x, y) => x + 1, _ + _)
  }

  log(s"Parallel time   - $partime ms")

}




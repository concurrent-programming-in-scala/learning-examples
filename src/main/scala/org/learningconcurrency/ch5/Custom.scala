package org.learningconcurrency
package ch5



import scala.collection.parallel._



class ParString(val str: String) extends immutable.ParSeq[Char] {
  def apply(i: Int) = str.charAt(i)
  
  def length = str.length
  
  def seq = new collection.immutable.WrappedString(str)
  
  def splitter = new ParStringSplitter(str, 0, str.length)
    
}


class ParStringSplitter(private val s: String, private var i: Int, private val limit: Int)
extends SeqSplitter[Char] {
  final def hasNext = i < limit
  final def next = {
    val r = s.charAt(i)
    i += 1
    r
  }
  def remaining = limit - i
  def dup = new ParStringSplitter(s, i, limit)
  def split = {
    val rem = remaining
    if (rem >= 2) psplit(rem / 2, rem - rem / 2)
    else Seq(this)
  }
  def psplit(sizes: Int*): Seq[ParStringSplitter] = {
    val ss = for (sz <- sizes) yield {
      val nlimit = (i + sz) min limit
      val ps = new ParStringSplitter(s, i, nlimit)
      i = nlimit
      ps
    }
    if (i == limit) ss else ss :+ new ParStringSplitter(s, i, limit)
  }
}


object CustomCharCount extends App {
  val txt = "A custom text " * 250000
  val partxt = new ParString(txt)
  
  val seqtime = warmedTimed(50) {
    txt.foldLeft(0)((n, c) => if (Character.isUpperCase(c)) n + 1 else n)
  }
  
  log(s"Sequential time - $seqtime ms")

  val partime = warmedTimed(50) {
    partxt.aggregate(0)((n, c) => if (Character.isUpperCase(c)) n + 1 else n, _ + _)
  }

  log(s"Parallel time   - $partime ms")

}




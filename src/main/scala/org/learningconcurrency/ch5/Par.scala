package org.learningconcurrency
package ch5






object ParBasic extends App {
  import scala.collection._

  val numbers = scala.util.Random.shuffle(Vector.tabulate(5000000)(i => i))

  val seqtime = timed {
    val n = numbers.max
    println(s"largest number $n")
  }

  log(s"Sequential time $seqtime ms")

  val partime = timed {
    val n = numbers.par.max
    println(s"largest number $n")
  }

  log(s"Parallel time $partime ms")
}


object ParUid extends App {
  import scala.collection._
  import java.util.concurrent.atomic._
  private val uid = new AtomicLong(0L)

  val seqtime = timed {
    for (i <- 0 until 10000000) uid.incrementAndGet()
  }
  log(s"Sequential time $seqtime ms")

  val partime = timed {
    for (i <- (0 until 10000000).par) uid.incrementAndGet()
  }
  log(s"Parallel time $partime ms")

}


object ParGeneric extends App {
  import scala.collection._
  import scala.io.Source

  def findLongestLine(xs: GenSeq[String]): Unit = {
    val line = xs.maxBy(_.length)
    log(s"Longest line - $line")
  }

  val doc = Array.tabulate(1000)(i => "lorem ipsum " * (i % 10))

  findLongestLine(doc)
  findLongestLine(doc.par)

}


object ParConfig extends App {
  import scala.collection._
  import scala.concurrent.forkjoin.ForkJoinPool

  val fjpool = new ForkJoinPool(2)
  val myTaskSupport = new parallel.ForkJoinTaskSupport(fjpool)
  val numbers = scala.util.Random.shuffle(Vector.tabulate(5000000)(i => i))
  val partime = timed {
    val parnumbers = numbers.par
    parnumbers.tasksupport = myTaskSupport
    val n = parnumbers.max
    println(s"largest number $n")
  }
  log(s"Parallel time $partime ms")  
}


object ParHtmlSpecSearch extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.collection._
  import scala.io.Source

  def getHtmlSpec() = Future {
    val specSrc: Source = Source.fromURL("http://www.w3.org/MarkUp/html-spec/html-spec.txt")
    try specSrc.getLines.toArray finally specSrc.close()
  }

  getHtmlSpec() foreach { case specDoc =>
    log(s"Download complete!")

    def search(d: GenSeq[String]) = warmedTimed() {
      d.indexWhere(line => line.matches(".*TEXTAREA.*"))
    }

    val seqtime = search(specDoc)
    log(s"Sequential time $seqtime ms")

    val partime = search(specDoc.par)
    log(s"Parallel time $partime ms")
  }

}


object ParNonParallelizableCollections extends App {
  import scala.collection._

  val list = List.fill(1000000)("")
  val vector = Vector.fill(1000000)("")
  log(s"list conversion time: ${timed(list.par)} ms")
  log(s"vector conversion time: ${timed(vector.par)} ms")
}


object ParNonParallelizableOperations extends App {
  import scala.collection._
  import scala.concurrent.ExecutionContext.Implicits.global
  import ParHtmlSpecSearch.getHtmlSpec

  getHtmlSpec() foreach { case specDoc =>
    def allMatches(d: GenSeq[String]) = warmedTimed() {
      val results = d.foldLeft("")((acc, line) => if (line.matches(".*TEXTAREA.*")) s"$acc\n$line" else acc)
      // Note: must use "aggregate" instead of "foldLeft"!
    }

    val seqtime = allMatches(specDoc)
    log(s"Sequential time - $seqtime ms")

    val partime = allMatches(specDoc.par)
    log(s"Parallel time   - $partime ms")
  }
}


object ParNonDeterministicOperation extends App {
  import scala.collection._
  import scala.concurrent.ExecutionContext.Implicits.global
  import ParHtmlSpecSearch.getHtmlSpec

  getHtmlSpec() foreach { case specDoc =>
    val seqresult = specDoc.find(line => line.matches(".*TEXTAREA.*"))
    val parresult = specDoc.par.find(line => line.matches(".*TEXTAREA.*"))
    log(s"Sequential result - $seqresult")
    log(s"Parallel result   - $parresult")
  }
}


object ParNonCommutativeOperator extends App {
  import scala.collection._
  
  val doc = mutable.ArrayBuffer.tabulate(20)(i => s"Page $i, ")
  def test(doc: GenIterable[String]) {
    val seqtext = doc.seq.reduceLeft(_ + _)
    val partext = doc.par.reduce(_ + _)
    log(s"Sequential result - $seqtext\n")
    log(s"Parallel result   - $partext\n")
  }
  test(doc)
  test(doc.toSet)
}


object ParNonAssociativeOperator extends App {
  import scala.collection._

  def test(doc: GenIterable[Int]) {
    val seqtext = doc.seq.reduceLeft(_ - _)
    val partext = doc.par.reduce(_ - _)
    log(s"Sequential result - $seqtext\n")
    log(s"Parallel result   - $partext\n")
  }
  test(0 until 30)
}


object ParMultipleOperators extends App {
  import scala.collection._
  import scala.concurrent.ExecutionContext.Implicits.global
  import ParHtmlSpecSearch.getHtmlSpec

  getHtmlSpec() foreach { case specDoc =>
    val length = specDoc.aggregate(0)(
      (count: Int, line: String) => count + line.length,
      (count1: Int, count2: Int) => count1 + count2
    )
    log(s"Total characters in HTML spec - $length")
  }
}


object ParSideEffectsIncorrect extends App {
  import scala.collection._

  def intSize(a: GenSet[Int], b: GenSet[Int]): Int = {
    var count = 0
    for (x <- a) if (b contains x) count += 1
    count
  }
  val seqres = intSize((0 until 1000).toSet, (0 until 1000 by 4).toSet)
  val parres = intSize((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)
  log(s"Sequential result - $seqres")
  log(s"Parallel result   - $parres")
}


object ParSideEffectsCorrect extends App {
  import scala.collection._
  import java.util.concurrent.atomic._

  def intSize(a: GenSet[Int], b: GenSet[Int]): Int = {
    val count = new AtomicInteger(0)
    for (x <- a) if (b contains x) count.incrementAndGet()
    count.get
  }
  val seqres = intSize((0 until 1000).toSet, (0 until 1000 by 4).toSet)
  val parres = intSize((0 until 1000).par.toSet, (0 until 1000 by 4).par.toSet)
  log(s"Sequential result - $seqres")
  log(s"Parallel result   - $parres")
}


object ParMutableWrong extends App {
  import scala.collection._

  val buffer = mutable.ArrayBuffer[Int]() ++= (0 until 250)
  for (x <- buffer.par) buffer += x
  log(buffer.toString)
}







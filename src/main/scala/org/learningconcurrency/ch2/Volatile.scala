package org.learningconcurrency
package ch2






object Volatile extends App {
  case class Page(txt: String, var position: Int)
  val pages = for (i <- 1 to 5) yield
    new Page("Na" * (100 - 20 * i) + " Batman!", -1)
  @volatile var found = false
  for (p <- pages) yield thread {
    var i = 0
    while (i < p.txt.length && !found)
      if (p.txt(i) == '!') {
        p.position = i
        found = true
      } else i += 1
  }
  while (!found) {}
  log(s"results: ${pages.map(_.position)}")
}


object VolatileScan extends App {
  val document: Seq[String] = for (i <- 1 to 5) yield "lorem ipsum " * (1000 - 200 * i) + "Scala"
  var results = Array.fill(document.length)(-1)
  @volatile var found = false
  val threads = for (i <- 0 until document.length) yield thread {
    def scan(n: Int, words: Seq[String], query: String): Unit =
      if (words(n) == query) {
        results(i) = n
        found = true
      } else if (!found) scan(n + 1, words, query)
    scan(0, document(i).split(" "), "Scala")
  }
  for (t <- threads) t.join()
  log(s"Found: ${results.find(_ != -1)}")
}


object VolatileUnprotectedUid extends App {

  @volatile var uidCount = 0L

  def getUniqueId() = {
    val freshUid = uidCount + 1
    uidCount = freshUid
    freshUid
  }

  def printUniqueIds(n: Int): Unit = {
    val uids = for (i <- 0 until n) yield getUniqueId()
    log(s"Generated uids: $uids")
  }

  val t = thread {
    printUniqueIds(5)
  }
  printUniqueIds(5)
  t.join()

}


object VolatileSharedStateAccess extends App {
  for (i <- 0 until 10000) {
    @volatile var t1started = false
    @volatile var t2started = false
    var t1index = -1
    var t2index = -1
  
    val t1 = thread {
      Thread.sleep(1)
      t1started = true
      t2index = if (t2started) 0 else 1
    }
    val t2 = thread {
      Thread.sleep(1)
      t2started = true
      t1index = if (t1started) 0 else 1
    }
  
    t1.join()
    t2.join()
    assert(!(t1index == 1 && t2index == 1), s"t1 = $t1index, t2 = $t2index")
  }
}


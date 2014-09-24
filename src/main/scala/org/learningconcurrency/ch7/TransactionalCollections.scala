package org.learningconcurrency
package ch7






object TransactionLocals extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._
  import CompositionSortedList._

  val myLog = TxnLocal("")

  def clearList(lst: TSortedList) = atomic { implicit txn =>
    while (lst.head() != null) {
      myLog() = myLog() + "\nremoved " + lst.head().elem
      lst.head() = lst.head().next()
    }
  }

  val myList = new TSortedList().insert(14).insert(22)
  def clearWithLog(): String = atomic { implicit txn =>
    clearList(myList) 
    myLog()
  }
  val f = Future { clearWithLog() }
  val g = Future { clearWithLog() }
  for (h1 <- f; h2 <- g) {
    log(s"Log for f: $h1\nLog for g: $h2")
  }

}


object TransactionalArray extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._

  val pages = Seq.fill(5)("Scala 2.10 is out, " * 7)
  val website = TArray(pages)

  def replace(pat: String, txt: String): Unit = atomic { implicit txn =>
    for (i <- 0 until website.length) website(i) = website(i).replace(pat, txt)
  }

  def asString = atomic { implicit txn =>
    var s: String = ""
    for (i <- 0 until website.length) s += s"Page $i\n=======\n${website(i)}\n\n"
    s
  }

  Future { replace("2.10", "2.11") }
  Thread.sleep(30)
  Future { replace("2.11", "2.12") }
  Thread.sleep(250)
  Future { log(s"Document\n$asString") }

}


object TransactionalMap extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._

  val tmap = TMap("a" -> 1, "B" -> 2, "C" -> 3)

  Future {
    atomic { implicit txn =>
      tmap("A") = 1
      tmap.remove("a")
    }
  }
  Thread.sleep(23)
  Future {
    val snap = tmap.single.snapshot
    log(s"atomic snapshot: $snap")
  }

}


object TransactionalDocument extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._

  class TDocument(val name: String) {
    val pages = Ref(TArray.ofDim[String](0))

    def addPage(page: String): this.type = atomic { implicit txn =>
      val oarray = pages()
      val narray = TArray.ofDim[String](oarray.length + 1)
      for (i <- 0 until oarray.length) narray(i) = oarray(i)
      narray(oarray.length) = page
      pages() = narray
      this
    }

    override def toString = atomic { implicit txn =>
      val array = pages()
      (0 until array.length).map(i => s"Page $i\n=======\n${array(i)}\n\n").mkString
    }
  }

  val doc = new TDocument("MyBook")
  doc.addPage("My Book Title")
  doc.addPage("Abstract: This is a book about concurrency.")

  Future { doc.addPage("Why is concurrency important.") }
  Future { doc.addPage("What concurrency means to you.") }
  Thread.sleep(250)
  Future { log(s"Document\n$doc") }

}


object TransactionalLibrary extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.concurrent.stm._
  import TransactionalDocument._

  class TLibrary {
    val documents = TMap.empty[String, TDocument]

    def addDoc(doc: TDocument): this.type = atomic { implicit txn =>
      if (documents.contains(doc.name)) sys.error("Document ${doc.name} already exists!")
      else documents(doc.name) = doc
      this
    }

    def removeDoc(name: String): Option[TDocument] = documents.single.remove(name)

    def findDocs(pattern: String): List[String] = atomic { implicit txn =>
      documents.filter({
        case (name, doc) =>
          val pages = doc.pages()
          (0 until pages.length).exists(i => pages(i).contains(pattern))
      }).keys.toList
    }
  }

  val library = new TLibrary()
  library.addDoc(new TDocument("README.md").addPage("Attention - important!"))

  Future {
    library.addDoc(new TDocument("ElManual.md").addPage("Es muy importante!"))
    Thread.sleep(5)
    library.removeDoc("README.md")
  }
  Future {
    val matches = library.findDocs("important").mkString("\n")
    log(s"Matches:\n$matches")
  }

}






package org.learningconcurrency
package ch4






object FuturesCreate extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global

  Future {
    log(s"the future is here")
  }

  log(s"the future is coming")

}


object FuturesType extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.io.Source

  val buildFile: Future[String] = Future {
    val f = Source.fromFile("build.sbt")
    try f.getLines.mkString("\n") finally f.close()
  }

  log(s"started reading build file asynchronously")
  log(s"status: ${buildFile.isCompleted}")
  Thread.sleep(250)
  log(s"status: ${buildFile.isCompleted}")
  log(s"status: ${buildFile.value}")

}


object FuturesCallbacks extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.io.Source

  val urlSpec: Future[Seq[String]] = Future {
    val f = Source.fromURL("http://www.w3.org/Addressing/URL/url-spec.txt")
    try f.getLines.toList finally f.close()
  }

  def find(lines: Seq[String], word: String) = lines.zipWithIndex collect {
    case (line, n) if line.contains(word) => (n, line)
  } mkString("\n")

  urlSpec onSuccess {
    case lines => log(s"Found occurrences of 'telnet'\n${find(lines, "telnet")}\n")
  }

  urlSpec onSuccess {
    case lines => log(s"Found occurrences of 'password'\n${find(lines, "password")}\n")
  }

  log("request started, continuing with other work")

}


object FuturesExceptions extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.io.Source

  val file = Future { Source.fromFile(".gitignore-SAMPLE").getLines.mkString("\n") }

  file onSuccess {
    case text => log(text)
  }

  file onFailure {
    case t => log(s"Failed due to $t")
  }

  import scala.util.{Try, Success, Failure}

  file onComplete {
    case Success(text) => log(text)
    case Failure(t) => log(s"Failed due to $t")
  }

}


object FuturesMap extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.io.Source
  import scala.util.Success

  val buildFile = Future { Source.fromFile("build.sbt").getLines }
  val gitignoreFile = Future { Source.fromFile(".gitignore-SAMPLE").getLines }

  val longestBuildLine = buildFile.map(lines => lines.maxBy(_.length))
  val longestGitignoreLine = for (lines <- gitignoreFile) yield lines.maxBy(_.length)

  longestBuildLine onComplete {
    case Success(line) => log(s"the longest line is '$line'")
  }

  longestGitignoreLine onFailure {
    case t => log(s"no longest line, because ${t.getMessage}")
  }
}


object FuturesFlatMap extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.io.Source

  val netiquette = Future { Source.fromURL("http://www.ietf.org/rfc/rfc1855.txt").mkString }
  val urlspec = Future { Source.fromURL("http://www.w3.org/Addressing/URL/url-spec.txt").mkString }
  val answer = for {
    nettext <- netiquette
    urltext <- urlspec
  } yield {
    "First of all, read this: " + nettext + " Once you're done, try this: " + urltext
  }

  answer onSuccess {
    case contents => log(contents)
  }

}


object FuturesRecover extends App {
  import scala.concurrent._
  import ExecutionContext.Implicits.global
  import scala.io.Source


}




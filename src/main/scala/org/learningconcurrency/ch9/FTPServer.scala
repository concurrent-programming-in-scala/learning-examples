package org.learningconcurrency
package ch9



import rx.lang.scala._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor._
import scala.concurrent.stm._
import java.io._
import org.apache.commons.io.FileUtils
import org.apache.commons.io.filefilter.TrueFileFilter
import scala.collection._
import scala.collection.convert.decorateAsScala._



class FileSystem(val rootpath: String) {
  val files = TMap[String, FileInfo]()

  def init() = atomic { implicit txn =>
    files.clear()

    val rootDir = new File(rootpath)
    val all = TrueFileFilter.INSTANCE
    val fileIterator = FileUtils.iterateFilesAndDirs(rootDir, all, all).asScala
    for (file <- fileIterator) {
      val info = FileInfo(file)
      files(info.name) = info
    }
  }

  def getFileList(dir: String): Map[String, FileInfo] = atomic { implicit txn =>
    files.filter(_._2.parent == dir)
  }

  def copyFile(srcpath: String, destpath: String): Unit = ???

  def deleteFile(srcpath: String): Unit = ???

  def fileStream(srcpath: String): Unit = ???

}


class FTPServerActor(fileSystem: FileSystem) extends Actor {
  import FTPServerActor._

  def receive = {
    case GetFileList(dir) =>
      val files = fileSystem.getFileList(dir)
      sender ! files
  }
}


object FTPServerActor {
  case class GetFileList(dir: String)
  case class CopyFile(srcpath: String, destpath: String)
  case class DeleteFile(path: String)
  case class DownloadFile(srcpath: String, destpath: String)

  def apply(fs: FileSystem) = Props(classOf[FTPServerActor], fs)
}


object FTPServer extends App {
  val fileSystem = new FileSystem(".")
  fileSystem.init()

  val port = args(0).toInt
  val actorSystem = ch8.remotingSystem("FTPServerSystem", port)
  val serverActor = actorSystem.actorOf(FTPServerActor(fileSystem))
}


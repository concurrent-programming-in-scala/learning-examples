package org.learningconcurrency
package ch9



import rx.lang.scala._
import scala.collection._
import scala.util.Try
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor._
import akka.pattern.pipe
import akka.event.Logging
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
      //if (info.isDir && info.path.contains("src")) println(info)
      files(info.path) = info
    }
  }

  def getFileList(dir: String): Map[String, FileInfo] = atomic { implicit txn =>
    files.filter(_._2.parent == dir)
  }

  def copyFile(srcpath: String, destpath: String): Unit = atomic { implicit txn =>
    ???
  }

  def deleteFile(srcpath: String): Unit = atomic { implicit txn =>
    import FileSystem._
    val info = files(srcpath)
    info.state match {
      case Copying(_) => sys.error("Cannot delete, file being copied.")
      case Deleted => sys.error("File already being deleted.")
      case Idle =>
        files(srcpath) = info.copy(state = Deleted)
        Txn.afterCommit { _ =>
          FileUtils.forceDelete(info.toFile)
          files.single.remove(srcpath)
        }
    }
  }

}


object FileSystem {
  sealed trait State
  case object Idle extends State
  case class Copying(n: Int) extends State
  case object Deleted extends State
}


class FTPServerActor(fileSystem: FileSystem) extends Actor {
  import FTPServerActor._

  val log = Logging(context.system, this)

  def receive = {
    case GetFileList(dir) =>
      //println(fileSystem.files.snapshot.map(_._2).filter(_.isDir).filter(_.path.contains("src")).mkString("\n"))
      val filesMap = fileSystem.getFileList(dir)
      val files = filesMap.map(_._2).to[Seq]
      sender ! files
    case DeleteFile(path) =>
      val f = Future {
        Try {
          fileSystem.deleteFile(path)
          path
        }
      }
      f pipeTo sender
  }
}


object FTPServerActor {
  sealed trait Command
  case class GetFileList(dir: String) extends Command
  case class CopyFile(srcpath: String, destpath: String) extends Command
  case class DeleteFile(path: String) extends Command

  def apply(fs: FileSystem) = Props(classOf[FTPServerActor], fs)
}


object FTPServer extends App {
  val fileSystem = new FileSystem(".")
  fileSystem.init()

  val port = args(0).toInt
  val actorSystem = ch8.remotingSystem("FTPServerSystem", port)
  val serverActor = actorSystem.actorOf(FTPServerActor(fileSystem), "server")
}


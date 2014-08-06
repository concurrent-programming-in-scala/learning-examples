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

  private def copyOnDisk(srcfile: File, destfile: File) = {
    FileUtils.copyFile(srcfile, destfile)
    atomic { implicit txn =>
      val ninfo = files(srcfile.getPath)
      files(srcfile.getPath) = ninfo.copy(state = ninfo.state.dec)
      files(destfile.getPath) = FileInfo(destfile)
    }
  }

  def copyFile(srcpath: String, destpath: String): String = atomic { implicit txn =>
    import FileSystem._
    val srcfile = new File(srcpath)
    val destfile = new File(destpath)
    val info = files(srcpath)
    if (files.contains(destpath)) sys.error(s"Destination $destpath already exists.")
    info.state match {
      case Created => sys.error(s"File $srcpath being created.")
      case Deleted => sys.error(s"File $srcpath already deleted.")
      case Idle | Copying(_) =>
        files(srcpath) = info.copy(state = info.state.inc)
        files(destpath) = FileInfo.creating(destfile, info.size)
        Txn.afterCommit { _ => copyOnDisk(srcfile, destfile) }
        srcpath
    }
  }

  def deleteFile(srcpath: String): String = atomic { implicit txn =>
    import FileSystem._
    val info = files(srcpath)
    info.state match {
      case Created => sys.error(s"File $srcpath not yet created.")
      case Copying(_) => sys.error(s"Cannot delete $srcpath, file being copied.")
      case Deleted => sys.error(s"File $srcpath already being deleted.")
      case Idle =>
        files(srcpath) = info.copy(state = Deleted)
        Txn.afterCommit { _ =>
          FileUtils.forceDelete(info.toFile)
          files.single.remove(srcpath)
        }
        srcpath
    }
  }

  def findFiles(regex: String): Seq[FileInfo] = {
    val snapshot = files.single.snapshot
    val infos = snapshot.values.toArray
    infos.par.filter(_.path.matches(regex)).seq
  }

}


object FileSystem {
  sealed trait State {
    def inc: State
    def dec: State
  }
  case object Created extends State {
    def inc = sys.error("File being created.")
    def dec = sys.error("File being created.")
  }
  case object Idle extends State {
    def inc = Copying(1)
    def dec = sys.error("Idle not copied.")
  }
  case class Copying(n: Int) extends State {
    def inc = Copying(n + 1)
    def dec = if (n > 1) Copying(n - 1) else Idle
  }
  case object Deleted extends State {
    def inc = sys.error("Cannot copy deleted.")
    def dec = sys.error("Deleted not copied")
  }
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
    case CopyFile(srcpath, destpath) =>
      Future {
        Try(fileSystem.copyFile(srcpath, destpath))
      } pipeTo sender
    case DeleteFile(path) =>
      Future {
        Try(fileSystem.deleteFile(path))
      } pipeTo sender
    case FindFiles(regex) =>
      Future {
        Try(fileSystem.findFiles(regex))
      } pipeTo sender
  }
}


object FTPServerActor {
  sealed trait Command
  case class GetFileList(dir: String) extends Command
  case class CopyFile(srcpath: String, destpath: String) extends Command
  case class DeleteFile(path: String) extends Command
  case class FindFiles(regex: String) extends Command

  def apply(fs: FileSystem) = Props(classOf[FTPServerActor], fs)
}


object FTPServer extends App {
  val fileSystem = new FileSystem(".")
  fileSystem.init()

  val port = args(0).toInt
  val actorSystem = ch8.remotingSystem("FTPServerSystem", port)
  val serverActor = actorSystem.actorOf(FTPServerActor(fileSystem), "server")
  val fileEventSubscription = fileSystemEvents(".").subscribe { event =>
    event match {
      case FileCreated(path) =>
        fileSystem.files.single(path) = FileInfo(new File(path))
      case FileDeleted(path) =>
        fileSystem.files.single.remove(path)
      case FileModified(path) =>
        fileSystem.files.single(path) = FileInfo(new File(path))
    }
  }
}


object FTPServerBench extends App {
  import org.scalameter._

  val fileSystem = new FileSystem(".")
  fileSystem.init()

  val runningTime = config(
    Key.exec.minWarmupRuns -> 100,
    Key.exec.maxWarmupRuns -> 200,
    Key.exec.benchRuns -> 1000,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default) measure {
    fileSystem.findFiles(".*ch5.*")
  }

  println("Running time: " + runningTime)

}









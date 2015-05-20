package org.learningconcurrency



import java.io._
import java.text.SimpleDateFormat
import org.apache.commons.io.FileUtils
import org.apache.commons.io.monitor._
import rx.lang.scala._



package ch9 {

  case class FileInfo(path: String, name: String, parent: String, modified: String, isDir: Boolean, size: Long, state: FileSystem.State) {
    def toRow = Array[AnyRef](name, if (isDir) "" else size / 1000 + "kB", modified)
    def toFile = new File(path)
  }

  object FileInfo {
    def apply(file: File): FileInfo = {
      val path = file.getPath
      val name = file.getName
      val parent = file.getParent
      val modified = dateFormat.format(file.lastModified)
      val isDir = file.isDirectory
      val size = if (isDir) -1 else FileUtils.sizeOf(file)
      FileInfo(path, name, parent, modified, isDir, size, FileSystem.Idle)
    }

    def creating(file: File, size: Long): FileInfo = {
      val path = file.getPath
      val name = file.getName
      val parent = file.getParent
      val modified = dateFormat.format(System.currentTimeMillis)
      val isDir = false
      FileInfo(path, name, parent, modified, isDir, size, FileSystem.Created)
    }
  }

  sealed trait FileEvent
  case class FileCreated(path: String) extends FileEvent
  case class FileDeleted(path: String) extends FileEvent
  case class FileModified(path: String) extends FileEvent

}


package object ch9 {

  val dateFormat = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")

  def fileSystemEvents(rootPath: String): Observable[FileEvent] = {
    Observable.apply { obs =>
      val fileMonitor = new FileAlterationMonitor(1000)
      val fileObs = new FileAlterationObserver(rootPath)
      val fileLis = new FileAlterationListenerAdaptor {
        override def onFileCreate(file: File) =
          obs.onNext(FileCreated(file.getPath))
        override def onFileChange(file: File) =
          obs.onNext(FileModified(file.getPath))
        override def onFileDelete(file: File) =
          obs.onNext(FileDeleted(file.getPath))
        override def onDirectoryCreate(file: File) =
          obs.onNext(FileCreated(file.getPath))
        override def onDirectoryChange(file: File) =
          obs.onNext(FileModified(file.getPath))
        override def onDirectoryDelete(file: File) =
          obs.onNext(FileDeleted(file.getPath))
      }
      fileObs.addListener(fileLis)
      fileMonitor.addObserver(fileObs)
      fileMonitor.start()

      Subscription { fileMonitor.stop() }
    }
  }

}



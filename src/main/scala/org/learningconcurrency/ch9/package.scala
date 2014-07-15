package org.learningconcurrency



import java.io._
import java.text.SimpleDateFormat
import org.apache.commons.io.FileUtils



package object ch9 {

}


package ch9 {

  sealed trait State
  case object Idle extends State
  case class Copying(n: Int) extends State
  case object Deleted extends State

  case class FileInfo(name: String, parent: String, modified: String, isDir: Boolean, size: Long, state: State)

  object FileInfo {
    def apply(file: File): FileInfo = {
      val name = file.getPath
      val parent = file.getParent
      val sdf = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss")
      val modified = sdf.format(file.lastModified)
      val isDir = file.isDirectory
      val size = if (isDir) -1 else FileUtils.sizeOf(file)
      FileInfo(name, parent, modified, isDir, size, Idle)
    }
  }

}


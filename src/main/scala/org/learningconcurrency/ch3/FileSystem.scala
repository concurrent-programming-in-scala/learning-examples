package org.learningconcurrency
package ch3



import java.io._
import java.util.concurrent._
import java.util.concurrent.atomic._
import scala.annotation.tailrec
import scala.collection._
import scala.collection.convert.decorateAsScala._
import org.apache.commons.io.FileUtils



class FileSystem {

  val actions: mutable.Buffer[String] =
    new mutable.ArrayBuffer[String]
    with mutable.SynchronizedBuffer[String]

  def logAction(msg: String): Unit = actions += msg

  sealed trait State

  class Idle extends State

  class Creating extends State

  class Copying(val n: Int) extends State

  class Deleting extends State

  class Entry(val isDir: Boolean) {
    val state = new AtomicReference[State](new Idle)
  }

  val files: concurrent.Map[String, Entry] =
    //new ConcurrentHashMap().asScala
    new concurrent.TrieMap()

  @tailrec private def prepareForDelete(entry: Entry): Boolean = {
    val s0 = entry.state.get
    s0 match {
      case i: Idle =>
        if (entry.state.compareAndSet(s0, new Deleting)) true
        else prepareForDelete(entry)
      case c: Creating =>
        logAction("File currently being created, cannot delete.")
        false
      case c: Copying =>
        logAction("File currently being copied, cannot delete.")
        false
      case d: Deleting =>
        false
    }
  }

  def deleteFile(filename: String): Unit = {
    files.get(filename) match {
      case None =>
        // file already deleted, nothing to do
      case Some(entry) if !entry.isDir =>
        logAction(s"Cannot delete - path '$filename' is a directory!")
      case Some(entry) =>
        execute {
          if (prepareForDelete(entry)) {
            if (FileUtils.deleteQuietly(new File(filename)))
              files.remove(filename, entry)
          }
        }
    }
  }

  @tailrec private def acquire(entry: Entry): Boolean = {
    val s0 = entry.state.get
    s0 match {
      case i: Idle =>
        if (entry.state.compareAndSet(s0, new Copying(1))) true
        else acquire(entry)
      case c: Creating =>
        logAction("File being created, cannot copy.")
        false
      case c: Copying =>
        if (entry.state.compareAndSet(s0, new Copying(c.n + 1))) true
        else acquire(entry)
      case d: Deleting =>
        logAction("File already deleted, cannot copy.")
        false
    }
  }

  @tailrec private def release(entry: Entry): Unit = {
    val s0 = entry.state.get
    s0 match {
      case i: Idle =>
        sys.error("Error - released more times than acquired.")
      case c: Creating =>
        sys.error("Error - released an entry that is now created.")
      case c: Copying if c.n <= 0 =>
        sys.error("Error - cannot have 0 or less copies in progress!")
      case c: Copying =>
        val newState = if (c.n == 0) new Idle else new Copying(c.n - 1)
        if (entry.state.compareAndSet(s0, newState)) {} // done
        else release(entry)
      case d: Deleting =>
        sys.error("Error - releasing a file that is being deleted!")
    }
  }

  def copyFile(src: String, dest: String): Unit = {
    files.get(src) match {
      case None =>
        logAction(s"File '$src' does not exist.")
      case Some(srcEntry) if !srcEntry.isDir =>
        sys.error(s"Path '$src' is a directory!")
      case Some(srcEntry) =>
        execute {
          if (acquire(srcEntry)) try {
            val destEntry = new Entry(false)
            destEntry.state.set(new Creating)
            if (files.putIfAbsent(dest, destEntry) == None) try {
              FileUtils.copyFile(new File(src), new File(dest))
            } finally release(destEntry)
          } finally release(srcEntry)
        }
    }
  }

  def filesInDir(dir: String): Iterable[String] = {
    // trie map snapshots
    for ((name, state) <- files; if name.startsWith(dir)) yield name
  }

}
package org.learningconcurrency
package ch9



import scala.collection._
import scala.swing._
import scala.swing.event._
import javax.swing.table._
import javax.swing._
import rx.lang.scala._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor._



abstract class FTPClientFrame extends MainFrame {
  import BorderPanel.Position._

  title = "ScalaFTP"

  class FilePane extends BorderPanel {
    object pathBar extends BorderPanel {
      val label = new Label("Path:")
      val filePath = new TextField("/")
      layout(label) = West
      layout(filePath) = Center
    }
    layout(pathBar) = North

    object scrollPane extends ScrollPane {
      val columnNames = Seq("Filename", "Size", "Date modified")
      val fileTable = new Table(Array[Array[Any]](), columnNames) {
        showGrid = true
      }
      contents = fileTable
    }
    layout(scrollPane) = Center

    object buttons extends GridPanel(1, 3) {
      val downloadButton = new Button("Download")
      val copyButton = new Button("Copy")
      val deleteButton = new Button("Delete")
      contents += downloadButton
      contents += copyButton
      contents += deleteButton
    }
    layout(buttons) = South
  }

  object files extends GridPanel(1, 2) {
    val leftPane = new FilePane
    val rightPane = new FilePane
    contents += leftPane
    contents += rightPane
  }

  object menu extends MenuBar {
    val menuFileExit = new MenuItem("Exit")
    val menuFile = new Menu("File") {
      contents += menuFileExit
    }
    val menuHelpAbout = new MenuItem("About...")
    val menuHelp = new Menu("Help") {
      contents += menuHelpAbout
    }
    contents += menuFile
    contents += menuHelp
  }

  object status extends BorderPanel {
    val label = new Label("")
    layout(new Label("Status: ")) = West
    layout(label) = Center
  }

  contents = new BorderPanel {
    layout(menu) = North
    layout(files) = Center
    layout(status) = South
  }

}


trait FTPClientApi {
  def getFileList(dir: String): Future[Seq[FileInfo]]
  //def copyFile(srcpath: String, destpath: String): Future[Unit]
  //def deleteFile(srcpath: String): Future[Unit]
  //def downloadFile(srcpath: String, destpath: String): Observable[Int]
}


trait FTPClientLogic {
  self: FTPClientFrame with FTPClientLogic =>

  
}


object FTPClient extends SimpleSwingApplication {

  try {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
  } catch {
    case e: Exception =>
      // ignore
      log(s"could not change look&feel: $e")
  }

  def top = new FTPClientFrame {}

  var serverUrl: String = ""

  override def main(args: Array[String]) {
    serverUrl = args(0)
    super.main(args)
  }

}


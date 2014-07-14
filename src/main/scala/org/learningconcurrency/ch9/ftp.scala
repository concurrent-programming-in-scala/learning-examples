package org.learningconcurrency



import scala.swing._
import scala.swing.event._
import javax.swing.table._
import rx.lang.scala._



abstract class FTPClientFrame extends MainFrame {
  import BorderPanel.Position._

  title = "ScalaFTP"

  class FilePane extends BorderPanel {
    val columnNames = Seq("Filename", "Size", "Date modified")
    val fileTable = new Table(Array[Array[Any]](), columnNames) {
      showGrid = true
    }
    val filePath = new TextField
    val pathBar = new BorderPanel {
      val label = new Label("Path:")
      layout(label) = West
      layout(filePath) = Center
    }
    layout(pathBar) = North
    val scrollPane = new ScrollPane {
      contents = fileTable
    }
    layout(scrollPane) = Center
  }

  class Files extends GridPanel(1, 2) {
    val leftPane = new FilePane
    val rightPane = new FilePane
    contents += leftPane
    contents += rightPane
  }

  class MainMenu extends MenuBar {
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

  val files = new Files
  val menu = new MainMenu

  contents = new BorderPanel {
    layout(menu) = North
    layout(files) = Center
  }

}


object FTPClient extends SimpleSwingApplication {

  def top = new FTPClientFrame {}

}


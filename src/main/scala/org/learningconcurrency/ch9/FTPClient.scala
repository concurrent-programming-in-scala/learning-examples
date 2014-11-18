package org.learningconcurrency
package ch9



import scala.collection._
import scala.util.{Try, Success, Failure}
import scala.swing._
import scala.swing.event._
import javax.swing.table._
import javax.swing._
import javax.swing.border._
import java.awt.Color
import java.io.File
import rx.lang.scala._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import akka.actor._
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import ch6._



abstract class FTPClientFrame extends MainFrame {
  import BorderPanel.Position._

  title = "ScalaFTP"

  class FilePane extends BorderPanel {
    object pathBar extends BorderPanel {
      val label = new Label("Path:")
      val filePath = new TextField(".") {
        border = BorderFactory.createLineBorder(Color.LIGHT_GRAY, 1, true)
        editable = false
      }
      val upButton = new Button("^")
      layout(label) = West
      layout(filePath) = Center
      layout(upButton) = East
      border = BorderFactory.createEmptyBorder(2, 2, 2, 2)
    }
    layout(pathBar) = North

    object scrollPane extends ScrollPane {
      val columnNames = Array[AnyRef]("Filename", "Size", "Date modified")
      val fileTable = new Table {
        showGrid = true
        model = new DefaultTableModel(columnNames, 0) {
          override def isCellEditable(r: Int, c: Int) = false
        }
        selection.intervalMode = Table.IntervalMode.Single
      }
      contents = fileTable
    }
    layout(scrollPane) = Center

    object buttons extends GridPanel(1, 2) {
      val copyButton = new Button("Copy")
      val deleteButton = new Button("Delete")
      contents += copyButton
      contents += deleteButton
    }
    layout(buttons) = South

    var parent: String = "."
    var dirFiles: Seq[FileInfo] = Nil

    def table = scrollPane.fileTable

    def currentPath = pathBar.filePath.text
  }

  object files extends GridPanel(1, 2) {
    val leftPane = new FilePane
    val rightPane = new FilePane
    contents += leftPane
    contents += rightPane

    def opposite(pane: FilePane) = {
      if (pane eq leftPane) rightPane else leftPane
    }
  }

  object menu extends MenuBar {
    object file extends Menu("File") {
      val exit = new MenuItem("Exit ScalaFTP")
      contents += exit
    }
    object help extends Menu("Help") {
      val about = new MenuItem("About...")
      contents += about
    }
    contents += file
    contents += help
  }
  
  object status extends BorderPanel {
    val label = new Label("connecting...", null, Alignment.Left)
    layout(new Label("Status: ")) = West
    layout(label) = Center
  }

  contents = new BorderPanel {
    layout(menu) = North
    layout(files) = Center
    layout(status) = South
  }

}


class FTPClientActor(implicit val timeout: Timeout) extends Actor {
  import FTPClientActor._
  import FTPServerActor._

  def unconnected: Actor.Receive = {
    case Start(host) =>
      // connect to server
      val serverActorPath = s"akka.tcp://FTPServerSystem@$host/user/server"
      val serverActorSel = context.actorSelection(serverActorPath)
      serverActorSel ! Identify(())
      context.become(connecting(sender))
  }

  def connecting(application: ActorRef): Actor.Receive = {
    case ActorIdentity(_, Some(ref)) =>
      application ! true
      println("found: " + ref)
      context.become(connected(ref))
    case ActorIdentity(_, None) =>
      application ! false
      context.become(unconnected)
  }

  def connected(serverActor: ActorRef): Actor.Receive = {
    case command: Command =>
      (serverActor ? command).pipeTo(sender)
  }

  def receive = unconnected

}


object FTPClientActor {
  case class Start(serverActorUrl: String)
}


trait FTPClientApi {
  implicit val timeout: Timeout = Timeout(4 seconds)
  val system = ch8.remotingSystem("FTPClientSystem", 0)
  val clientActor = system.actorOf(Props(classOf[FTPClientActor], timeout))

  def host: String

  val connected: Future[Boolean] = {
    val f = clientActor ? FTPClientActor.Start(host)
    f.mapTo[Boolean]
  }

  def getFileList(dir: String): Future[(String, Seq[FileInfo])] = {
    val f = clientActor ? FTPServerActor.GetFileList(dir)
    f.mapTo[Seq[FileInfo]].map(fs => (dir, fs))
  }

  def copyFile(srcpath: String, destpath: String): Future[String] = {
    val f = clientActor ? FTPServerActor.CopyFile(srcpath, destpath)
    f.mapTo[Try[String]].map {
      case Success(s) => s
      case Failure(t) => throw t
    }
  }

  def deleteFile(srcpath: String): Future[String] = {
    val f = clientActor ? FTPServerActor.DeleteFile(srcpath)
    f.mapTo[Try[String]].map {
      case Success(s) => s
      case Failure(t) => throw t
    }
  }

}


trait FTPClientLogic {
  self: FTPClientFrame with FTPClientApi =>

  connected.onComplete {
    case Success(true) =>
      swing {
        status.label.text = "Connected!"
        refreshPane(files.leftPane)
        refreshPane(files.rightPane)
      }
    case Success(false) =>
      swing { status.label.text = "Could not find server." }
    case Failure(t) =>
      swing { status.label.text = s"Could not connect to server: $t" }
  }

  def updatePane(pane: FilePane, dir: String, files: Seq[FileInfo]): Unit = {
    val table = pane.scrollPane.fileTable
    table.model match {
      case d: DefaultTableModel =>
        d.setRowCount(0)
        pane.parent = if (dir == ".") "." else dir.take(dir.lastIndexOf(File.separator))
        pane.dirFiles = files.sortBy(!_.isDir)
        for (f <- pane.dirFiles) d.addRow(f.toRow)
    }
  }

  def refreshPane(pane: FilePane): Unit = {
    val dir = pane.pathBar.filePath.text
    getFileList(dir) onComplete {
      case Success((dir, files)) =>
        swing { updatePane(pane, dir, files) }
      case Failure(t) =>
        swing { status.label.text = s"Could not update file pane: $t" }
    }
  }

  def setupPane(pane: FilePane): Unit = {
    val fileClicks = pane.table.rowDoubleClicks.map(row => pane.dirFiles(row))
    fileClicks.filter(_.isDir).subscribe { fileInfo =>
      pane.pathBar.filePath.text = pane.pathBar.filePath.text + File.separator + fileInfo.name
      refreshPane(pane)
    }
    pane.pathBar.upButton.clicks.subscribe { _ =>
      pane.pathBar.filePath.text = pane.parent
      refreshPane(pane)
    }

    def rowActions(button: Button): Observable[FileInfo] = button.clicks
      .map(_ => pane.table.peer.getSelectedRow)
      .filter(_ != -1)
      .map(row => pane.dirFiles(row))
    def setStatus(txt: String) = {
      status.label.text = txt
      refreshPane(files.leftPane)
      refreshPane(files.rightPane)
    }

    val rowCopies = rowActions(pane.buttons.copyButton)
      .map(info => (info, files.opposite(pane).currentPath))
    rowCopies.subscribe { t =>
      val (info, destDir) = t
      val dest = destDir + File.separator + info.name
      copyFile(info.path, dest) onComplete {
        case Success(s) =>
          swing { setStatus(s"File copied: $s") }
        case Failure(t) =>
          swing { setStatus(s"Could not copy file: $t")}
      }
    }

    val rowDeletes = rowActions(pane.buttons.deleteButton)
    rowDeletes.subscribe { info =>
      deleteFile(info.path) onComplete {
        case Success(s) =>
          swing { setStatus(s"File deleted: $s") }
        case Failure(t) =>
          swing { setStatus(s"Could not delete file: $t") }
      }
    }
  }

  setupPane(files.leftPane)
  setupPane(files.rightPane)

  menu.file.exit.reactions += {
    case ButtonClicked(_) =>
      system.stop(clientActor)
      system.shutdown()
      sys.exit(0)
  }

  menu.help.about.reactions += {
    case ButtonClicked(_) =>
      Dialog.showMessage(message = "ScalaFTP version 0.1, made in Switzerland", title = "About ScalaFTP")
  }

}


object FTPClient extends SimpleSwingApplication {

  try {
    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
  } catch {
    case e: Exception =>
      // ignore
      log(s"could not change look&feel: $e")
  }

  def top = new FTPClientFrame with FTPClientApi with FTPClientLogic {
    def host = hostArg
  }

  var hostArg: String = ""

  override def main(args: Array[String]) {
    hostArg = if (args.length > 0) args(0) else {
      println("usage (from sbt):")
      println("    run <ftp-server-url>")
      println("    runMain org.learningconcurrency.ch9.FTPClient <ftp-server-url>")
      sys.exit(1)
    }
    super.main(args)
  }

}


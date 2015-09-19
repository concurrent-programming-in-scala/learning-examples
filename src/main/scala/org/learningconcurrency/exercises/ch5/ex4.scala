package org.learningconcurrency
package exercises
package ch5

import javafx.scene.layout.HBox

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout.{Pane, VBox}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

object Ex4 extends JFXApp {

  /**
   * Implement a program that simulates a cellular automaton in parallel.
   */

  val maxX = 40
  val maxY = 40

  case class Cell(state: Boolean, index: Int, neighbors: Seq[Int])

  def index(i: Int, j: Int) = i + j * maxX

  def findNeighbors(x: Int, y: Int) = {
    for {
      i <- x - 1 to x + 1
      j <- y - 1 to y + 1
      if i > -1 && i < maxX && j > -1 && j < maxY && ((i != x) || (j != x))
    } yield index(i, j)
  }

  def checkInitialState(x: Int, y: Int) = (x == maxX / 2 - 1 || x == maxX / 2 + 1) && y == maxY / 2
  
  def initialize: IndexedSeq[Cell] = {
    (
      for {
        x <- 0 until maxX
        y <- 0 until maxY
      } yield (x, y)
      ).map {
      case (x, y) => Cell(
        state = checkInitialState(x,y),
        index = index(x, y),
        neighbors = findNeighbors(x, y)
      )
    }
  }

  def countNeighbors(cell: Cell, cells: Seq[Cell]) =
    cell.neighbors.count((i) => cells(i).state)

  def defState(countNeighbors: Int) = {
    (countNeighbors == 2) || (countNeighbors == 3)
  }

  def next(cells: Seq[Cell]) = {
    cells.par.map(
      (cell) => cell.copy(state = defState(countNeighbors(cell, cells)))).toIndexedSeq
  }

  //Test App
  
  var cells = List(initialize)

  val cellAreaWidth = 400
  val cellAreaHeight = 400

  val rectWidth = cellAreaWidth / maxX
  val rectHeight = cellAreaHeight / maxY

  def buildRectangles = (
    for {
      y <- 0 until maxY
      x <- 0 until maxX
    } yield (x, y)).
    map {
    case (posX, posY) => new Rectangle() {
      x = posX * rectWidth
      y = posY * rectHeight
      width = rectWidth
      height = rectHeight
      fill = Color.GRAY
    }
  }.toArray


  val rectangles = buildRectangles

  val cellsPane = new Pane {
    maxWidth = cellAreaWidth
    maxHeight = cellAreaHeight
  }
  rectangles.foreach(cellsPane.children.add(_))

  def drawCells = cells.head.foreach((c) => rectangles(c.index).setFill(if (c.state) Color.RED else Color.WHITE))

  val nextButton = new Button() {
    text = "=>"
    onAction = handle {
      cells = next(cells.head) :: cells
      drawCells
    }
  }

  val prevButton = new Button() {
    text = "<="
    onAction = handle {
      cells = cells match {
        case h :: Nil => cells
        case h :: t => t
      }
      drawCells
    }
  }

  val hbox = new HBox()
  hbox.children.add(prevButton)
  hbox.children.add(nextButton)


  val vbox = new VBox {
    padding = Insets(20)
    spacing = 10
  }

  vbox.children.add(hbox)
  vbox.children.add(cellsPane)

  stage = new PrimaryStage {
    title.value = "Ch5 Ex4"
    scene = new Scene {
      content = vbox
    }
  }

  stage.sizeToScene()

  drawCells

}

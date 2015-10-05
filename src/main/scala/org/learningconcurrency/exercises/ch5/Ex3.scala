package org.learningconcurrency
package exercises
package ch5

import scala.annotation.tailrec
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

/**
 * Implement a program that renders the Mandelbrot set in parallel.
 */

object Ex3 extends JFXApp {

  val width = 400
  val height = 400

  val minX = -1.75
  val maxX = 0.8

  val minY = -1.2
  val maxY = 1.2

  val stepX = (maxX - minX) / width
  val stepY = (maxY - minY) / height

  val threshold = 300

  case class RGB(red: Int, green: Int, blue: Int)

  case class Pixel(x: Int, y: Int, gray: Double)

  def buildPixel(x: Int, y: Int) = {
    val xc = minX + stepX * x
    val yc = minY + stepY * y

    val count = calcIterations(x = 0, y = 0, xc = xc, yc = yc, i = 0)
    
    Pixel(x = x, y = y, gray = 1.0 - count * 1.0 / threshold)
  }

  @tailrec
  def calcIterations(x: Double, y: Double, xc: Double, yc: Double, i: Int): Int = {

    if ((x * x + y * y < 2) && (i < threshold))
      calcIterations(
        x = x * x - y * y + xc,
        y = 2 * x * y + yc,
        xc = xc,
        yc = yc,
        i  = i + 1
      )
    else {
      i
    }
  }

  //render set
  val pixels = (for {
    x <- 0 until width
    y <- 0 until height
  } yield (x,y)).par.map {
    case (x,y) => buildPixel(x,y)
  }

  //build javaFX rectangle
  val rectangles = pixels.map {
    case p: Pixel => new Rectangle() {
      x = p.x
      y = p.y
      width = 1
      height = 1
      fill = Color.gray(p.gray)
    }
  }

  val pane = new Pane {
    content = rectangles.toList
  }

  //build scene
  stage = new JFXApp.PrimaryStage {
    title.value = "Ch5 Ex3"
    scene = new Scene {
      fill = Color.gray(1)
      content = pane
    }
  }

  stage.setHeight(width)
  stage.setWidth(height)

}

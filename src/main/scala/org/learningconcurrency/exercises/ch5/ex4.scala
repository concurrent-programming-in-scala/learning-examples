package org.learningconcurrency.exercises.ch5

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import java.util.concurrent.Executors
import javax.imageio.ImageIO

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.StdIn
import scala.util.{Success, Try}

/**
  * Implement a program that simulates a cellular automaton in parallel.
  */
object Ex4 extends App {

  import org.learningconcurrency.ch5.warmedTimed

  trait CellularAutomator {
    type State
    type Locality
    type Field <: Seq[State]
    type Position
    type Rule = Locality => State

    def rule: Rule

    def localitySize: Int

    def calculateNextState(currentState: Field): Field

    def calculateNextStates(currentState: Field, n: Int): Iterator[Field]
  }

  object Rule30 extends OneDimentionCellularAutomator#Rule {
    override def apply(locality: Byte): Boolean = locality match {
      case 7 /*111*/ => false
      case 6 /*110*/ => false
      case 5 /*101*/ => false
      case 4 /*100*/ => true
      case 3 /*011*/ => true
      case 2 /*010*/ => true
      case 1 /*001*/ => true
      case 0 /*000*/ => false
      case v => throw new IllegalArgumentException(s"Value must be < 8 (with 3 cell counts), but $v found")
    }
  }

  trait OneDimentionCellularAutomator extends CellularAutomator {
    override type State = Boolean
    override type Locality = Byte
    override type Field = Seq[Boolean]
    override type Position = Int

    private val doubleLocality = localitySize * 2

    protected def localityStates(pos: Position, currentState: Seq[Boolean]): Locality = {
      currentState
        .slice(pos - localitySize, pos + localitySize + 1)
        .zipWithIndex
        .foldLeft(0) {
          case (r, (v, index)) => if (v) r | 1 << doubleLocality - index else r
        }.toByte
    }
  }

  class SingleThreadOneDimentionCellularAutomator(override val rule: Byte => Boolean,
                                                  override val localitySize: Int) extends OneDimentionCellularAutomator {
    override def calculateNextState(currentState: Seq[Boolean]): Seq[Boolean] = {
      for {
        x <- currentState.indices
      } yield rule(localityStates(x, currentState))
    }

    override def calculateNextStates(currentState: Seq[Boolean], n: Int): Iterator[Seq[Boolean]] = {
      Iterator.iterate(currentState)(calculateNextState).take(n)
    }
  }

  class FutureMultiThreadOneDimentionCellularAutomator(segments: Int,
                                                       override val rule: Byte => Boolean,
                                                       override val localitySize: Int)(implicit ec: ExecutionContext) extends OneDimentionCellularAutomator {
    override def calculateNextState(currentState: Seq[Boolean]): Seq[Boolean] = {
      // todo запускать рассчет соседей и после этого рассчет этой же точки в следующей генерации
      val segmentSize = currentState.size / segments
      val fs = for {
        segment <- 1 to segments
      } yield Future {
        (segment, for {
          x <- (segment - 1) * segmentSize until segment * segmentSize
        } yield rule(localityStates(x, currentState)))
      }
      Await.result(Future.sequence(fs), Duration.Inf).sortBy(_._1).flatMap(_._2)
    }

    override def calculateNextStates(currentState: Seq[Boolean], n: Int): Iterator[Seq[Boolean]] = {
      Iterator.iterate(currentState)(calculateNextState).take(n)
    }
  }

  class ImageSaver(format: String, filePath: String) {
    def apply(field: Iterator[Seq[Boolean]], size: Int, steps: Int) = {
      val bi = new BufferedImage(size, steps, BufferedImage.TYPE_BYTE_BINARY)

      val g = bi.createGraphics()
      g.setPaint(Color.WHITE)

      for {
        (xs, y) <- field.zipWithIndex
        (v, x) <- xs.zipWithIndex
        if !v
      } {
        g.drawLine(x, y, x, y)
      }

      ImageIO.write(bi, format, new File(filePath))
    }
  }

  val singleThreadAutomator = new SingleThreadOneDimentionCellularAutomator(Rule30, 1)

  val processors = Runtime.getRuntime.availableProcessors()

  val pool = Executors.newFixedThreadPool(processors * 4)
  implicit val ec = ExecutionContext.fromExecutor(pool)
  val futureMultiThreadAutomator = new FutureMultiThreadOneDimentionCellularAutomator(processors * 10, Rule30, 1)

  val startState = {
    val size = 50000
    val a = Array.fill(size)(false)
    a(size / 2) = true
    a
  }

  val steps = 100
  val warmUps = 10

  print("Warmup single thread")
  val singleThreadTime = warmedTimed(warmUps) {
    print(".")
    singleThreadAutomator.calculateNextStates(startState, steps).toList
  }
  println

  println(s"Single thread calculation time: $singleThreadTime")

  print("Warmup future multi thread")
  val futureMultiThreadTime = warmedTimed(warmUps) {
    print(".")
    futureMultiThreadAutomator.calculateNextStates(startState, steps).toList
  }
  println

  println(s"Future multi thread calculation time: $futureMultiThreadTime")

  println("Print result to console? [y/n]")
  Try(StdIn.readChar()) match {
    case Success('y') =>
      val result = futureMultiThreadAutomator.calculateNextStates(startState, steps)
      result.foreach(field => println(field.map(v => if (v) '▉' else ' ').mkString))
    case _ =>
  }

  println("Save result to PNG file? [y/n]")
  Try(StdIn.readChar()) match {
    case Success('y') =>
      val result = futureMultiThreadAutomator.calculateNextStates(startState, steps)
      val fileName = "cellular-automaton.png"
      new ImageSaver("PNG", fileName).apply(result, startState.length, steps)
      println(s"See '$fileName' file with cellular automaton evolution image in project folder")
    case _ =>
  }

  pool.shutdown()
}

package org.learningconcurrency.exercises.ch5

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import java.util.concurrent.Executors
import javax.imageio.ImageIO

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.StdIn
import scala.util.{Success, Try}

/**
  * Implement a program that renders the Mandelbrot set in parallel.
  */
object Ex3 extends App {

  import org.learningconcurrency.ch5.warmedTimed

  case class MandelbrotParams(imageWidth: Int,
                              imageHeight: Int,
                              xMin: Double,
                              xMax: Double,
                              yMin: Double,
                              yMax: Double,
                              maxIterations: Int) {
    val xStep = if (xMin >= 0 && xMax >= 0) {
      (xMin + xMax) / imageWidth
    } else if (xMin < 0 && xMax >= 0) {
      (xMax - xMin) / imageWidth
    } else {
      (-xMin + xMax) / imageWidth
    }

    val yStep = if (yMin >= 0 && yMax >= 0) {
      (yMin + yMax) / imageHeight
    } else if (xMin < 0 && yMax >= 0) {
      (yMax - yMin) / imageHeight
    } else {
      (-yMin + yMax) / imageHeight
    }
  }

  type MandelbrotSet = Seq[Seq[Int]]

  trait MandelbrotSetBuilder {
    def apply(params: MandelbrotParams): Future[MandelbrotSet]

    protected final def calculateMandelbrotElement(point: Complex,
                                                   maxIterations: Int = 1000
                                                  ): Int = {
      @tailrec
      def iterate(i: Int = 0, z: Complex = Complex(0, 0)): Int = {
        if (i < maxIterations && z.abs < 4) {
          val newPoint = z.sqr + point
          iterate(i + 1, newPoint)
        } else {
          // todo Either[Unit, Int]
          if (i == maxIterations) -1 else i
        }
      }
      iterate()
    }
  }

  case class Complex(x: Double, y: Double) {
    def abs: Double = x * x + y * y

    def sqr: Complex = Complex(x * x - y * y, 2 * x * y)

    def +(c: Complex): Complex = Complex(x + c.x, y + c.y)
  }

  class SingleThreadBlockingGenerator extends MandelbrotSetBuilder {
    override def apply(params: MandelbrotParams): Future[Seq[Seq[Int]]] = {
      import params._

      val result = for {
        y0 <- 0 until imageHeight
      } yield for {
        x0 <- 0 until imageWidth
      } yield {
        val xToCheck = xMin + x0 * xStep
        val yToCheck = yMin + y0 * yStep
        val complexValueToCheck = Complex(xToCheck, yToCheck)
        calculateMandelbrotElement(complexValueToCheck, maxIterations = params.maxIterations)
      }
      Future.successful(result)
    }
  }

  class MultiThreadGenerator(segments: Int)(implicit ec: ExecutionContext) extends MandelbrotSetBuilder {
    override def apply(params: MandelbrotParams): Future[Seq[Seq[Int]]] = {
      import params._

      val segmentHeight = params.imageHeight / segments

      val fs = for {
        segment <- 1 to segments
      } yield {
        Future {
          (segment, for {
            y0 <- ((segment - 1) * segmentHeight) to (segment * segmentHeight)
          } yield {
            for {
              x0 <- 0 until imageWidth
            } yield {
              val xToCheck = xMin + x0 * xStep
              val yToCheck = yMin + y0 * yStep
              val complexValueToCheck = Complex(xToCheck, yToCheck)
              calculateMandelbrotElement(complexValueToCheck, maxIterations = params.maxIterations)
            }
          })
        }
      }

      Future.sequence(fs).map(_.sortBy(_._1).flatMap(_._2))
    }
  }

  class MandelbrotSetImageSaver(format: String, filePath: String) {
    def apply(params: MandelbrotParams, set: MandelbrotSet): Unit = {
      val bi = new BufferedImage(params.imageWidth, params.imageHeight, BufferedImage.TYPE_INT_RGB)

      val g = bi.createGraphics()

      import params._

      val histogram = set.flatten
        .groupBy(identity)
        .map(g => (g._1, g._2.size))
        .filter(_._1 >= 0)
        .map(g => (g._1 - 1, g._2))
        .withDefaultValue(0)

      val total = histogram.values.sum

      for {
        px <- 0 until imageWidth
        py <- 0 until imageHeight
      } {
        val numIters = set(py)(px)

        var colorVal = 0f
        for (i <- 0 until numIters) {
          colorVal += histogram(i) / total.toFloat
        }
        val rgb = Color.HSBtoRGB(0.1f + colorVal, 1.0f, colorVal * colorVal)

        g.setPaint(new Color(rgb))
        g.drawLine(px, py, px, py)
      }

      ImageIO.write(bi, format, new File(filePath))
    }
  }

  val processors = Runtime.getRuntime.availableProcessors()

  val pool = Executors.newFixedThreadPool(processors * 4)
  implicit val ec = ExecutionContext.fromExecutor(pool)

  val singleThreadGenerator = new SingleThreadBlockingGenerator
  val multiThreadGenerator = new MultiThreadGenerator(segments = processors * 10)

  val params = MandelbrotParams(900, 600, -2f, 1f, -1f, 1f, 1000)

  val warmupTimes = 30

  print("Warmup single thread")
  val singleThreadGeneratorTime = warmedTimed(warmupTimes) {
    print(".")
    singleThreadGenerator(params)
  }
  println

  println(s"Single thread generator time: $singleThreadGeneratorTime")

  print("Warmup future multi thread")
  val multiThreadGeneratorTime = warmedTimed(warmupTimes) {
    print(".")
    Await.result(multiThreadGenerator(params), Duration.Inf)
  }
  println

  println(s"Multi thread generator time: $multiThreadGeneratorTime")

  println("Save result to PNG file? [y/n]")
  Try(StdIn.readChar()) match {
    case Success('y') =>
      val mandelbrotElements = Await.result(multiThreadGenerator(params), Duration.Inf)
      val fileName = "mandelbrot.png"
      new MandelbrotSetImageSaver("PNG", fileName).apply(params, mandelbrotElements)
      println(s"See '$fileName' file with mandelbrot set in project folder")
    case _ =>
  }
  pool.shutdown()
}
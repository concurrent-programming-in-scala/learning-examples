package org.learningconcurrency
package exercises
package ch5

import scala.util.Random

/**
 * Count the occurrences of the whitespace character in a randomly generated string,
 * where the probability of a whitespace at each position is determined by a p parameter.
 *
 * Use the parallel foreach method.
 *
 * Plot a graph that correlates the running time of this operation with the p parameter.
 */
object Ex2 extends App {

  import org.learningconcurrency.ch5._

  var r = new Random

  val chars = ('a' to 'z') ++ ('A' to 'Z')

  def generateSymbol(p: Double): Char =
    if (r.nextDouble() > p) chars(Random.nextInt(chars.length)) else ' '

  def generateString(p: Double, length: Int = 10000): Seq[Char] = {
    (0 to length).map((i) => generateSymbol(p))
  }

  def timedForeach(s: Seq[Char]) = {
    var count = 0
    def add = synchronized {
      count += 1
    }

    warmedTimed(times = 400) {
      s.par.foreach((s) => if (s == ' ') add)
    }
  }

  def timedCount(s: Seq[Char]) = {
    warmedTimed(times = 400) {
      s.par.count(_ == ' ')
    }
  }

  //probability
  val p = (0 until 10).map { i => i / 9.0 }

  log("---- Calculation occurrences with foreach method")
  val dataForeach = p.map((p) => (p, generateString(p))).map {
    case (p, s) => log(s"p = $p"); (p, timedForeach(s))
  }

  log("---- Calculation occurrences with count method")
  val dataCount = p.map((p) => (p, generateString(p))).map {
    case (p, s) => log(s"p = $p"); (p, timedCount(s))
  }

  //plot graph
  //uses https://github.com/quantifind/wisp

  import com.quantifind.charts.Highcharts._

  hold
  line(dataForeach)
  line(dataCount)
  title("Ch5 Ex2")
  legend(Seq("foreach method", "count method"))
  xAxis("probability")
  yAxis("time (ms)")
}

package org.learningconcurrency
package exercises
package ch3

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.util.regex.Pattern

import scala.sys.process._

/**
 * Implement a method spawn that, given a block of Scala code, starts a new JVM process and runs the specified block in the new process:
 * def spawn[T](block: =>T): T = ???
 * Once the block returns a value, the spawn method should return the value from the child process.
 * If the block throws an exception, the spawn method should throw the same exception.
 */
object Ex8 extends App {

  // This method's preconditions are the following:
  //   - In case of executing in sbt, set `fork` setting to `true` (set fork := true ).
  //
  // If passed block which contains `System.exit`, this method throws `SecurityException`.
  def spawn[T](block: => T): T = {
    val className = Ex8_EvaluationApp.getClass().getName().split((Pattern.quote("$")))(0)
    val tmp = File.createTempFile("concurrent-programming-in-scala", null)
    tmp.deleteOnExit()

    val out = new ObjectOutputStream(new FileOutputStream(tmp))
    try {
      out.writeObject(() => block)
    } finally {
      out.close()
    }

    val ret = Process(s"java -cp ${System.getProperty("java.class.path")} $className ${tmp.getCanonicalPath}").!
    if (ret != 0)
      throw new RuntimeException("fails to evaluate block in a new JVM process")

    val in = new ObjectInputStream(new FileInputStream(tmp))
    try {
      in.readObject() match {
        case e: Throwable => throw e
        case x => x.asInstanceOf[T]
      }
    } finally {
      in.close()
      tmp.delete()
    }
  }

  val s1 = spawn({
    1 + 1
  })
  assert(s1 == 2)

  try {
    spawn({
      "test".toInt
    })
  } catch {
    case e: NumberFormatException =>
    case _: Throwable => assert(false)
  }

  try {
    spawn({
      System.exit(0)
    })
  } catch {
    case e: SecurityException =>
    case _: Throwable => assert(false)
  }
}
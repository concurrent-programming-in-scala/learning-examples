package org.learningconcurrency
package exercises
package ch3

import java.net.ServerSocket
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.util.regex.Pattern
import java.net.Socket

import scala.sys.process._

/**
 * Implement a method spawn that, given a block of Scala code, starts a new JVM process and runs the specified block in the new process:
 * def spawn[T](block: =>T): T = ???
 * Once the block returns a value, the spawn method should return the value from the child process.
 * If the block throws an exception, the spawn method should throw the same exception.
 */
object Ex8 extends App {

  // This method's preconditions are the following:
  //   - the `scala` command is added to the `PATH` variable.
  //   - In case of executing in sbt, set `fork` setting to `true` (set fork := true ).
  def spawn[T](block: => T): T = {
    val className = Ex8_EvaluationServer.getClass().getName().split((Pattern.quote("$")))(0)
    val lines = Process(s"scala -cp ${System.getProperty("java.class.path")} $className").lineStream

    val port = lines.head.toInt // wait for outputting port

    val socket = new Socket("127.0.0.1", port)
    try {
      val out = new ObjectOutputStream(socket.getOutputStream())
      out.writeObject(() => block) // wrap `block` not to be evaluated.
      val in = new ObjectInputStream(socket.getInputStream())
      in.readObject() match {
        case e: Throwable => throw e
        case x => x.asInstanceOf[T]
      }
    } finally {
      socket.close()
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
}
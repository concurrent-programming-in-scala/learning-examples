package org.learningconcurrency
package exercises
package ch3

import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.security.Permission

// This application receives the file path in which the serialized `Function0` object has been written.
// Then, it reads and evaluates serialized `Function0` object, finally overwrites its result to the same file.
object Ex8_EvaluationApp extends App {
  System.setSecurityManager(new SecurityManager() {
    // allows access to file.
    override def checkPermission(perm: Permission): Unit = {}
    // detects `System.exit` in order not to be halted by the caller.
    override def checkExit(status: Int): Unit = {
      throw new SecurityException("not allowed to pass a block which contains System.exit(int) !")
    }
  });

  val path = args(0)

  val in = new ObjectInputStream(new FileInputStream(path))
  try {
    val f0 = in.readObject().asInstanceOf[Function0[Any]]
    in.close()

    val out = new ObjectOutputStream(new FileOutputStream(path))
    try {
      out.writeObject(f0())
    } catch {
      case e: Throwable => out.writeObject(e)
    } finally {
      out.close()
    }
  } finally {
    in.close()
  }
}
package org.learningconcurrency.exercises.ch3

import java.net.ServerSocket
import java.io.ObjectInputStream
import java.io.ObjectOutputStream

import scala.language.reflectiveCalls

// This server
//   - tells client the bound port by means of standard output.
//   - expects client to transfer `Function0` object.
object Ex8_EvaluationServer extends App {
  using(new ServerSocket(0)) { serverSocket =>
    println(serverSocket.getLocalPort()) // tells client the bound port.

    using(serverSocket.accept()) { socket =>
      val in = new ObjectInputStream(socket.getInputStream())
      val f0 = in.readObject().asInstanceOf[Function0[Any]]

      val out = new ObjectOutputStream(socket.getOutputStream())
      try {
        out.writeObject(f0())
      } catch {
        case e: Throwable => out.writeObject(e)
      }
    }
  }

  def using[A, R <: { def close() }](resource: R)(f: R => A): A =
    try {
      f(resource)
    } finally {
      resource.close()
    }
}
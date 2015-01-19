package org.learningconcurrency
package exercises
package ch1

object Ex2 extends App {

  def fuse[A, B](a: Option[A], b: Option[B]): Option[(A, B)] = for {
    aVal <- a
    bVal <- b
  } yield (aVal, bVal)

}

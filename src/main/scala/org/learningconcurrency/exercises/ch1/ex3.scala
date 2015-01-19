package org.learningconcurrency
package exercises
package ch1

object Ex3 extends App {

  def check2[T](xs: Seq[T])(pred: T => Boolean): Boolean = xs.forall { x =>
    try {
      pred(x)
    } catch { 
      case _: Exception => false
    }
  }

}

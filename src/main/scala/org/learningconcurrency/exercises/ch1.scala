package org.learningconcurrency.exercises

object CH1Solutions extends App{

  def compose[A, B, C](g: B => C, f: A => B): A => C = g compose f

  def fuse[A, B](a: Option[A], b: Option[B]): Option[(A, B)] = for {
    aVal <- a
    bVal <- b
  } yield (aVal, bVal)

  def check2[T](xs: Seq[T])(pred: T => Boolean): Boolean = xs.forall {x => 
    try {
      pred(x)
    } catch { 
      case _:Exception => false
    }
  )

  def permutations(x: String): Seq[String] = x.permutations.toList
    
}

case class Pair[P, Q](first: P, second: Q)

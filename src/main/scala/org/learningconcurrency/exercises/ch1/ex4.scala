package org.learningconcurrency
package exercises
package ch1

// for those who consider the same characters different elements
object Ex4a extends App {

  def permutations(s: String): Seq[String] = {
    if (s.length == 0) Seq("")
    else for {
      i <- 0 until s.length
      q <- permutations(s.take(i) + s.takeRight(s.length - i - 1))
    } yield s(i) + q
  }

  println(permutations("abba"))

}


// for those who consider the same characters the same elements
object Ex4b extends App {

  def permutations(s: String): Seq[String] = {
    if (s.length == 0) Seq("")
    else {
      for {
        i <- s.map(c => s.indexOf(c)).toSet[Int].toSeq
        q <- permutations(s.take(i) + s.takeRight(s.length - i - 1))
      } yield s(i) + q
    }
  }

  println(permutations("abba"))

}


// for those who in love with the standard library :)
object Ex4c extends App {

  def permutations(x: String): Seq[String] = x.permutations.toList
  
  println(permutations("abba"))

}

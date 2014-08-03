package org.learningconcurrency
package ch4





// // only enable with Scala 2.10.4
// object Finagle extends App {
//   import com.twitter.util.{Future, Promise}

//   val tombola = Future {
//     scala.util.Random.shuffle((0 until 10000).toVector)
//   }

//   tombola onSuccess { numbers =>
//     log(s"And the winner is: ${numbers.head}")
//   }

// }




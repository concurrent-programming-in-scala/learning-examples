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

//   tombola onSuccess { numbers =>
//     log(s"Once, more, the winner is: ${numbers.head}")
//   }

// }


object Scalaz extends App {
  import scalaz.concurrent._

  val tombola = Future {
    scala.util.Random.shuffle((0 until 10000).toVector)
  }

  tombola.runAsync { numbers =>
    log(s"And the winner is: ${numbers.head}")
  }

  tombola.runAsync { numbers =>
    log(s"... ahem, winner is: ${numbers.head}")
  }

}


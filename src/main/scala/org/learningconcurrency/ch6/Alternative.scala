package org.learningconcurrency
package ch6



import scala.reactive._



object AkkaStreams extends App {
  import akka.actor.ActorSystem
  import akka.stream._
  import akka.stream.scaladsl._

  implicit val system = ActorSystem("system")

  val numbers = Iterator.from(1).take(100)

  def isPrime(n: Int) = (2 to (n - 1)).forall(n % _ != 0)

  Flow(numbers)
    .filter(isPrime)
    .map(num => s"Prime number: $num")
    .foreach(println)
    .onComplete(FlowMaterializer(MaterializerSettings())) {
      case _ => system.shutdown()
    }

}


object ReactiveCollections extends App {

  def isPrime(n: Int) = (2 to (n - 1)).forall(n % _ != 0)

  val numbers = new Reactive.Emitter[Int]
  numbers
    .filter(isPrime)
    .map(num => s"Prime number: $num")
    .foreach(println)

  for (i <- 0 until 100) numbers += i

}

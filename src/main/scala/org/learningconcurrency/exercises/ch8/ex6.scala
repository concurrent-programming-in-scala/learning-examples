package org.learningconcurrency
package exercises
package ch8

import akka.actor.{Props, ActorSystem, Actor, ActorRef}
import akka.pattern._
import akka.util.Timeout

import scala.collection.immutable.IndexedSeq
import scala.concurrent.duration._

import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future
import scala.util.Success
import scala.util.Failure

/**
 * A distributed hash map is a collection distributed across multiple computers,
 * each of which contains part of the data, called a shard.
 * When there are 2^n shards, the first n bits of the hash code of the key are used
 * to decide which shard a key-value pair should go to.
 *
 * Implement the distributed hash map with the DistributedMap class:
 * class DistributedMap[K, V](shards: ActorRef*) {
 * def update(key: K, value: V): Future[Unit] = ???
 * def get(key: K): Future[Option[V]] = ???
 * }
 *
 * The DistributedMap class takes a list of actor references to the ShardActor instances,
 * whose actor template you also need to implement.
 *
 * You might assume that the length of the shards list is a power of two.
 * The update and get methods are asynchronous, and return the result in a future object.
 */
object Ex6 extends App {

  case class DistributedKey(shard: Int, key: Int)

  import org.learningconcurrency.exercises.ch8.Ex6.MapActor._

  class DistributedMap[K, V](shards: ActorRef*) {

    implicit val timeout: Timeout = 5 seconds

    val shardsArray = shards.toArray
    val n = (Math.log10(shardsArray.length) / Math.log10(2)).toInt

    def getDistributedKey(key: Int, n: Int): DistributedKey = {

      def getKey(bs: String, n: Int): DistributedKey = {
        DistributedKey(Integer.parseInt(bs.takeRight(n), 2),
          if (bs.length > n) Integer.parseInt(bs.take(bs.length - n), 2) else 0)
      }

      getKey(key.toBinaryString, n)
    }

    def update(key: K, value: V): Future[Unit] = {
      val dk = getDistributedKey(key = key.hashCode, n = n)
      (shardsArray(dk.shard) ? Update(dk.key, value)).mapTo[Unit]
    }

    def get(key: K): Future[Option[V]] = {
      val dk = getDistributedKey(key = key.hashCode, n = n)
      (shardsArray(dk.shard) ? Get(dk.key)).mapTo[Option[V]]
    }
  }

  class MapActor[V] extends Actor {

    var m = Map.empty[Int, V]

    override def receive: Receive = {
      case Get(key: Int) =>
        sender() ! m.get(key)
      case Update(key, value: V) =>
        log(s"update $key -> $value, actor = ${this.self.path} ")
        m = m + (key -> value)
    }
  }

  object MapActor {

    case class Get(key: Int)

    case class Update[V](key: Int, value: V)

    val props = Props(classOf[MapActor[String]])
  }

  //test

  val system = ActorSystem("distributedSystem")
  val actors = (0 until 4).map((i) => system.actorOf(MapActor.props, s"mapActor-$i"))

  val distributedMap = new DistributedMap[Int, String](actors: _*)

  val values = List((0, "A"), (1, "B"), (2, "C"), (3, "D"), (4, "E"), (5,"F"), (6,"G"))

  values foreach { case (k, v) => distributedMap.update(k, v) }

  val fl = values map ((x) => distributedMap.get(x._1))
  fl foreach ((f) => f foreach { case v => log(s"v = $v") })

  Thread.sleep(5000)
  system.shutdown()
}

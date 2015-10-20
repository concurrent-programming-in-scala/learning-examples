package org.learningconcurrency
package exercises
package ch8

import akka.actor._
import akka.pattern._

import scala.concurrent.duration._
import scala.util.{Failure, Success}

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Recall the bank account example from Chapter 2, Concurrency on the JVM and the Java Memory Model.
 * Implement different bank accounts as separate actors,
 * represented with the AccountActor class.
 * When an AccountActor class receives a Send message,
 * it must transfer the specified amount of money to the target actor.
 *
 * What will happen if either of the actors receives a Kill message
 * at any point during the money transaction?
 */
object Ex2 extends App {

  import org.learningconcurrency.exercises.ch8.Ex2.AccountActor._

  class AccountActor(name: String, var money: Int) extends Actor {
    override def receive: Receive = {
      case Send(amount, target) =>
        if (money >= amount) {
          log(s"send $amount")
          (target ? Add(amount))(5 seconds) onComplete {
            case Success(v) => money -= amount
              log(s"Success: Add $amount to $target ($v).")
            case Failure(e) => log(s"Error: Add $amount to $target ($e).")
          }
        } else {
          log(s"Insufficient funds. ($money < $amount)")
        }
      case Add(amount) => log(s"ADD $amount")
        money += amount
        sender() ! "Ok"
      case Print => log(s"$name: $money")
    }
  }

  object AccountActor {

    case class Send(amount: Int, target: ActorRef)

    case class Add(amount: Int)

    case object Print

    def props(name: String, money: Int) = Props(classOf[AccountActor], name, money)
  }


  //test

  val system = ActorSystem("AccountSystem")


  import org.learningconcurrency.exercises.ch8.Ex2.AccountActor._

  val first = system.actorOf(AccountActor.props("Account A", 10), "account_A")
  val second = system.actorOf(AccountActor.props("Account B" ,0), "account_B")

  first ! Send(100, second)

  Thread.sleep(5000)

  first ! Print
  second ! Print

  Thread.sleep(5000)

  system.shutdown()

}

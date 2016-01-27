package org.learningconcurrency
package exercises
package ch8

import akka.actor._
import akka.event.Logging
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

  /**
    * If the actor receives a Kill message at any point during the transaction,
    * The actor will restart and it will lost his state (amount)
    * This problem can't be solved easily in the general case.
   */
  class AccountActor(name: String, var money: Int) extends Actor {

    val log = Logging(context.system, this)

    override def receive: Receive = {
      case AccountActor.PlusMoney(amount) =>
        money += amount
        sender ! AccountActor.Ok
      case AccountActor.MinusMoney(amount) if money >= amount =>
        money -= amount
        sender ! AccountActor.Ok
      case AccountActor.MinusMoney(amount) =>
        log.error(s"Insufficient funds. ($money < $amount)")
        sender ! AccountActor.Error
      case AccountActor.Print => log.info(s"$name: $money")
    }
  }

  object AccountActor {
    case class PlusMoney(amount: Int)
    case class MinusMoney(amount: Int)

    case object Ok
    case object Error

    case object Print

    def props(name: String, money: Int) = Props(classOf[AccountActor], name, money)
  }

  class TransactionActor extends Actor {

    val log = Logging(context.system, this)

    def checkTransferTo: Receive = {
      case AccountActor.Ok => log.info("Transfer complete")
    }

    def checkTransferFrom(accountTo: ActorRef, amount: Int): Receive = {
      case AccountActor.Ok =>
        accountTo ! AccountActor.PlusMoney(amount)
        context.become(checkTransferTo)
      case AccountActor.Error =>
        log.error("Transfer error (from)")
        context.stop(self)
    }

    override def receive: Actor.Receive = {
      case TransactionActor.StartTransaction(accountFrom, accountTo, amount) =>
        accountFrom ! AccountActor.MinusMoney(amount)
        context.become(checkTransferFrom(accountTo, amount))
    }
  }

  object TransactionActor {
    case class StartTransaction(accountFrom: ActorRef, accountTo: ActorRef, amount: Int)
  }

  //test
  val system = ActorSystem("AccountSystem")

  val first = system.actorOf(AccountActor.props("Account A", 10), "account_A")
  val second = system.actorOf(AccountActor.props("Account B", 0), "account_B")

  val transaction = system.actorOf(Props[TransactionActor], "transfer")
  transaction ! TransactionActor.StartTransaction(accountFrom = first, accountTo = second, amount = 7)

  Thread.sleep(1000)

  first ! AccountActor.Print
  second ! AccountActor.Print

  Thread.sleep(2000)

  system.shutdown()

}

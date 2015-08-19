package org.learningconcurrency
package exercises
package ch2

import org.learningconcurrency.ch2._

object Ex7 extends App {

  import SynchronizedProtectedUid._

  class Account(val name: String, var money: Int) {
    val uid = getUniqueId()
  }

  def send(a1: Account, a2: Account, n: Int) {
    def adjust() {
      a1.money -= n
      a2.money += n
    }

    if (a1.uid < a2.uid) {
      a1.synchronized {
        a2.synchronized {
          adjust()
        }
      }
    } else {
      a2.synchronized {
        a1.synchronized {
          adjust()
        }
      }
    }
  }


  def sendAll(accounts: Set[Account], target: Account): Unit = {

    def adjust() = {
      target.money = accounts.foldLeft(0)((s, a) => {
        val money = a.money
        a.money = 0
        s + money
      }
      )
    }

    def sendAllWithSynchronize(la: List[Account]): Unit = la match {
      case h :: t => h synchronized {
        sendAllWithSynchronize(t)
      }
      case _ => adjust()
    }

    sendAllWithSynchronize((target :: accounts.toList).sortBy(_.uid))
  }

  val accounts = (1 to 100).map((i) => new Account(s"Account: $i",i*10)).toSet
  val target = new Account("Target account", 0)

  sendAll(accounts,target)

  accounts.foreach((a) => log(s"${a.name}, money = ${a.money}"))
  log(s"${target.name} - money = ${target.money}")

}

package org.learningconcurrency
package ch6






object SubjectsPublish extends App {
  import rx.lang.scala._
  import scala.concurrent.duration._
  import ObservablesCreateAsynchronous._

  object OS {
    val messageBus = Subject[String]()
    messageBus.subscribe(log _)
  }

  object KernelModuleA {
    val systemClock = Observable.interval(1 seconds).map(t => s"systime: $t")
  }

  object KernelModuleB {
    val fileSystem = modifiedFiles(".").map(filename => s"file modification: $filename")
  }

  val loadedModules = List(
    KernelModuleA.systemClock.subscribe(OS.messageBus),
    KernelModuleB.fileSystem.subscribe(OS.messageBus)
  )

  log(s"RxOS booted!")
  Thread.sleep(10000)
  for (mod <- loadedModules) mod.unsubscribe()
  log(s"RxOS going for shutdown")

}


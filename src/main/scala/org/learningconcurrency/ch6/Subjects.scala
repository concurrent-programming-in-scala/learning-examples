package org.learningconcurrency
package ch6






object SubjectsOS extends App {
  import rx.lang.scala._
  import scala.concurrent.duration._
  import ObservablesSubscriptions._

  object RxOS {
    val messageBus = Subject[String]()
    messageBus.subscribe(log _)
  }

  object TimeModule {
    val systemClock = Observable.interval(1.seconds).map(t => s"systime: $t")
  }

  object FileSystemModule {
    val fileModifications = modifiedFiles(".").map(filename => s"file modification: $filename")
  }

  log(s"RxOS booting...")
  val modules = List(
    TimeModule.systemClock,
    FileSystemModule.fileModifications
  )
  val loadedModules = modules.map(_.subscribe(RxOS.messageBus))
  log(s"RxOS boot sequence finished!")

  Thread.sleep(10000)
  for (mod <- loadedModules) mod.unsubscribe()
  log(s"RxOS going for shutdown")

}


object SubjectsOSLog extends App {
  import rx.lang.scala._
  import SubjectsOS.{TimeModule, FileSystemModule}

  object RxOS {
    val messageBus = Subject[String]()
    val messageLog = subjects.ReplaySubject[String]()
    messageBus.subscribe(log _)
    messageBus.subscribe(messageLog)
  }

  val loadedModules = List(
    TimeModule.systemClock,
    FileSystemModule.fileModifications
  ).map(_.subscribe(RxOS.messageBus))

  log(s"RxOS booting")
  Thread.sleep(1000)
  log(s"RxOS booted!")
  Thread.sleep(10000)
  for (mod <- loadedModules) mod.unsubscribe()
  log(s"RxOS dumping the complete event log")
  RxOS.messageLog.subscribe(log _)
  log(s"RxOS going for shutdown")

}


object SubjectsOSRegistry extends App {
  import rx.lang.scala._

  object KernelModuleC {
    private val newKeys = Subject[(String, String)]()
    val registry = subjects.BehaviorSubject(Map[String, String]())
    newKeys.scan(Map[String, String]())(_ + _).subscribe(registry)
    def add(kv: (String, String)) = newKeys.onNext(kv)
  }

  KernelModuleC.registry.subscribe(reg => log(s"App A sees registry $reg"))

  log("RxOS about to add home dir")
  Thread.sleep(1000)
  KernelModuleC.add("dir.home" -> "/home/")

  object KernelModuleD {
    type Reg = Map[String, String]
    val registryDiffs = KernelModuleC.registry.scan((prev: Reg, curr: Reg) => curr -- prev.keys).drop(1)
  }
  KernelModuleD.registryDiffs.subscribe(diff => log(s"App B detects registry change: $diff"))

  log("RxOS about to add root dir")
  Thread.sleep(1000)
  KernelModuleC.add("dir.root" -> "/root/")

}


object SubjectsAsync extends App {
  import rx.lang.scala._

  object ProcessModule {
    private val added = Subject[Either[Int, Int]]()
    private val ended = Subject[Either[Int, Int]]()
    private val events = (added merge ended).scan(Set[Int]()) {
      case (set, Right(pid)) => set + pid
      case (set, Left(pid)) => set - pid
    }
    val processes = subjects.AsyncSubject[Set[Int]]()
    events.subscribe(processes)
    def add(pid: Int) = added.onNext(Right(pid))
    def end(pid: Int) = ended.onNext(Left(pid))
  }

  ProcessModule.add(1)
  ProcessModule.add(2)
  ProcessModule.add(3)

  log("RxOS processes started")
  Thread.sleep(1000)
  log("RxOS going for shutdown!")

  ProcessModule.end(1)
  ProcessModule.processes.subscribe(pids => log(s"need to force-kill processes ${pids.mkString(",")}"))
  Thread.sleep(1000)
  ProcessModule.processes.onCompleted()

}


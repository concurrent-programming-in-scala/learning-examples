package org.learningconcurrency
package ch6






object SubjectsOS extends App {
  import rx.lang.scala._
  import scala.concurrent.duration._
  import ObservablesSubscriptions._

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

  val loadedModules =
    for (mod <- List(KernelModuleA.systemClock, KernelModuleB.fileSystem)) yield
      mod.subscribe(OS.messageBus)

  log(s"RxOS booted!")
  Thread.sleep(10000)
  for (mod <- loadedModules) mod.unsubscribe()
  log(s"RxOS going for shutdown")

}


object SubjectsOSLog extends App {
  import rx.lang.scala._
  import SubjectsOS.{KernelModuleA, KernelModuleB}

  object OS {
    val messageLog = subjects.ReplaySubject[String]()
  }

  val loadedModules = 
    for (mod <- List(KernelModuleA.systemClock, KernelModuleB.fileSystem)) yield {
      OS.messageLog.onNext(s"loading module $mod")
      mod.subscribe(OS.messageLog)
    }

  log(s"RxOS booting")
  Thread.sleep(1000)
  log(s"RxOS booted!")
  Thread.sleep(10000)
  for (mod <- loadedModules) mod.unsubscribe()
  log(s"RxOS dumping the complete event log")
  OS.messageLog.subscribe(log _)
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

